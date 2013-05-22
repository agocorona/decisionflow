-----------------------------------------------------------------------------
--
-- Module      :  WFGenerator
-- Copyright   :  Alberto Gómez Corona
-- License     :  AllRightsReserved
--
-- Maintainer  :  Alberto Gómez Corona agocorona@gmail.com
-- Stability   :  experimental
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------
{-# OPTIONS
            -XRecordWildCards
            -XScopedTypeVariables
            -XDeriveDataTypeable
            -XNoMonomorphismRestriction
            -XCPP
            #-}

-- #define TEST
module WFGenerator (
 getConfigureWF,getConfigureWF', confSuffix, addVoteFlows, restartVoteFlows
) where
import DataDefs
import MFlow.Wai.Blaze.Html.All
#ifdef TEST
    hiding (ask,askt, select,content,div)
import MFlow.Forms.Test
#endif
import qualified Text.Blaze.Html5 as El
import qualified Text.Blaze.Html5.Attributes as At
import Data.Typeable
import qualified Control.Workflow as WF
import Control.Monad.Trans
import Data.TCache.IndexQuery as Q
import Data.TCache.IndexText
import Data.Persistent.Collection
import GHC.Conc
import Data.List (isPrefixOf)
import Data.Monoid
import qualified Data.Vector as V
import Data.RefSerialize hiding ((<|>))
--import Control.Workflow.Patterns
import Control.Workflow hiding (step)
import Control.Monad
import Unsafe.Coerce
import qualified Data.Map as M
import qualified Data.ByteString.Lazy.Char8 as B


{-
orientado a hacer un workflow general
    documents - protos
    groups - creategroup (pueda elegir usuarios)
    sendto user, group - criterio de votacion
        en caso de group dar la opcion de dejarlo al criterio del grupo
        la forma de votacion(2)

    otros sendto

    criterio de votación yes/no chooseOptions
         puede elegirlo el WF o el grupo

   tiene que haber un grupo de autores,
                   un grupo de revisores que votan
                   que hacer segun el resultado

   por tanto un workflow se puede descomponer en workflows
    que envian de un grupo a tros.

    sendToGroupAs category
     grupos activos que tienen categorias se les envia con sendToGroupAs category
           esas categorias estan asociadas a workflows
                esos workflows pueden llamar a otros grupos inactivos o activos
     grupos inactivos gobernados por workflows
     grupos virtuales, que duran mientras dura el Workflow (1)


al elegir autores
    dar a elegir el crear un grupo
    o crearlo automáticamente?
    un grupo puede estar formado por usuarios y grupos?
          Si incluye un grupo
              como recibe un WF externo?(2)
      optimamente si
      pueden ser privados
        como se le nombra?
          con el nombre del WF que se está creando?
          crearlo como estructura permanente o solo temporal? (1)
              preguntar al usuario


un wfname puede recibir cualquier proto?
 siempre que tenga el mismosistema de votación
 porque el sistema de votacion esta definido por el WF
 donde se almacena los parametros de votacion? en el WF o en el grupo?
 no puede ir en votation porque
 si no hay grupo, los parametros tienen que estar en el subject of en el wf
 hay que linkar usuario a wf

dilema:
   el workflow atraviesa grupos o
   grupos que se comunican y cada uno tiene workflows
 hay alguna forma de unificar ambos?
   un workflow puede extenderse a varios grupos,
      pero considerar a cada uno una caja negra

   un workflow puede extenderse a varios workflows
      cada workflow es una caja negra
        los grupos son pasivos.

   los grupos son activos, no hay workflow sin grupo que lo controle

que cada grupo tenga unas categorias/workflows para peticiones externas
   identificada por el nombre del workflow peticionario.

   usage case:
     un workflow para la aprobacion de documentos
       puede que estipule la aprobación del grupo de los jefes
         con ciertos criterios de mayoria
       pero se puede hacer que el grupo de jefes decida autonomamente como aprobar
         en ese caso tendrán un wf. interno de aprobación y unos procedimientos.

   Pero a la hora de crear un wf, elgrupo de jefes se define inicialmente como pasivo
   como se define un grupo pasivo y como se activa un grupo?
       (de unidad administrativa a unidad politica)

     un grupo activo tendria una categoria/wf para cada wf externo
       por tanto, un grupo se activa si se crea ese wf interno
       bajo que condiciones se activa?
         el wf externo tiene un dueño
         el grupo tiene otro dueño, que puede ser el mismo
           el autor del wf tiene que pedir permiso al autor del grupo para enviarle wf
              espera hasta que este  creada categoria para wf externo

       cuando el grupo esta creado por el autor del WF, es decision de éste.
       el wf llamante contempla al grupo como un usuario
       el wf llamado crea un wf interno que llama a los usuarios, los consulta y
          y devuelve el resultado al llamante.

     como se computan los resultados? los computa el WF interno o el externo?
       es decir, el criterio de aceptacion de un documento o lo marca el wf interno o el externo?
          el resultado lo computa el wf interno para no tener que cambiarlo cuando se activa

     como se determina el resultado de la votacion
        estructura votation es abstracta.
          parametros de votacion:
             Si/no
                 porcentaje de mayoria
                 porcentaje de bloqueo
             chooseOptions - se edita el el proto las opciones
                 numero de opciones que saldrán. o
                 votos minimos
          esos parametros se editan en el proto
             se asignan al wf interno del grupo
        en cualquier caso:
           votacion se crea en proto
           se edita cuando se usa el proto en WFGenerator del WF externo
           los parametros de porcentaje de mayoria, necesario y complain se piden al
              hacer el WF.
           se translada al WF interno.
           se vota en cuando se usa
estructuras:
  WF tiene owner(s)
  group tiene owner(s)
  group se crean cuando se eligen usuarios en un grupo.
     usuarios se hace miembros de ese grupo (y cola asociada)
     se crea un wf intermo de consulta para responder al wf externo en ese grupo.
       como se guarda?
  un group tiene WFs
  un WF crea groups por defecto con WFs internos (1)

  grupos pueden ser miembros de grupos y los usuarios ven las propuestas de la jerarquia


añadir timeout, enganche con otros wfs

como se guarda el proto
  en WFRef wfconf
  en wf
    ejecutando exec $ step newWFRef "wf"

Que pasa si se cambia el WF de un grupo subordinado?
  al reiniciar el WF
-}
--askw :: (Serialize a,Typeable a)=> View Html IO a -> FlowM Html WIO a

askw = step . ask

confSuffix= "conf"

type WIO= WF.Workflow IO


newtype AmendPage = AmendPage ( View Html IO ()) deriving Typeable

setAmendPage html= setSessionData $ AmendPage html
getAmendPage= getSessionData `onNothing` return (AmendPage mempty) >>= \(AmendPage h) -> return h
addAmendPage html= do
      html' <- getAmendPage
      let html''= html' `mappend` html
      setAmendPage html''
      return html''

getConfigureWF
  :: MonadIO m => String -> String -> FlowM Html m (View Html IO (), Subject -> WIO ())
getConfigureWF wfname key=do
   (amend,wf) <-  getConfigureWF' wfname key
   return(amend,wfAddPrelude wfname wf)
   where
   wfAddPrelude swfname wf s= do
     ref <- WF.step . atomically $ do
       Right ref <- getWFRef 0 swfname s
       writeWFRef ref (setStatus s Processing)
       return ref
     wf ref

getConfigureWF'
  :: MonadIO m => String -> String -> FlowM Html m (View Html IO (), WFRef Subject -> WIO ())
getConfigureWF' wfname key= runFlowIn (wfname++ confSuffix) $ do

    grauthors <- step . askt  (const $ getDBRef $ keyGroup "test2" )  $ selectGroup wfname "Who can create it"

    hist <- addAmendPage
        ( p <<< (pre << ("this group create the proposals: "++ show grauthors)
        ++> wlink () (b << "edit"))
        `waction`
        (const $ do askt (const grauthors) $ (edit grauthors <++ br  <** submitButton "submit")
                    breturn ()))

    sub <- step $ do
                WFProto _ _ proto <- chooseProtos
                askt (const proto{swfname=wfname}) $   hist
                    **> b << "Edit the proto"
                    ++> (edit proto  >>= \pr -> return pr{swfname=wfname})

                    <++ br
                    <** submitButton "submit"

    amend <- addAmendPage
            $   p <<< (pre << ("with this template: " ++ description sub)
            ++> wlink () (b << "edit")
            `waction` (const $ (askt (const sub) $ create sub) >> breturn ()))

    rproto <- atomic $ newDBRef $ WFProto key wfname sub

    f <- loop wfname  rproto " to vote the request" 0

--    WFProto _ _ sub <- atomic $ readDBRef rproto `onNothing` noRef rproto

    step $ assignWF grauthors wfname

    return (amend,f)

    where

    assignWF rgr wfname = atomic $ do
          gr <- readDBRef rgr  `onNothing` noRef rgr
          writeDBRef rgr gr{ pcategories= wfname:pcategories gr }


    loop :: String -> DBRef WFProto -> String -> Int ->  FlowM Html WIO (WFRef Subject ->  WIO ())
    loop wfname  rproto  msg level = do

      rgroup <- step . askt (const . getDBRef $ keyGroup "test2" ) $ selectGroup wfname msg

      hist <- addAmendPage ( p <<< (pre << (spaces level++msg++" "++ show rgroup)
                       ++> wlink () (b << "edit") `waction` (const $ (askt (const rgroup) $ edit rgroup <++ br <** submitButton "submit") >> breturn ())))


      vot <- step $ do
          n <- atomic  $   readDBRef rgroup `onNothing` noRef rgroup
                       >>= return . length . M.elems . pusers
          let votproto= setNElems n votationProto                             !> ("nelems= " ++ show n)

          askt (const votproto) $ hist **> p << b << "Votation parameters" ++> create votproto <++ br <** submitButton "submit"

      hist <- addAmendPage
        $   p <<< pre << (spaces level++"with these votation parameters: "++ show vot)
        ++> wlink () (b << "edit") `waction` (const $ editVotInProto level rproto >> breturn ())

      step . atomic $ do
         WFProto g c proto <- readDBRef rproto `onNothing` noRef rproto
         let proto'= addVotationElem vot proto
         writeDBRef rproto $ WFProto g c proto'

      s <- askw  $ (hist **> b << "send to a queue or group and finalize"
                    ++> getString Nothing)
                    <|> br
                    ++> wlink "sel" (b <<  "or select groups to send depending on the results")

      when (s /= "sel") $ addAmendPage ( p <<< wraw (pre << (spaces level ++ "send to this queue: "++ show s))) >> return ()

      wf  <- getInternalWF wfname rgroup
      options <- liftIO $ getOptions vot
      let defineBranch option= do
                let msg= "users/groups to send if the following option is chosen:" ++ option
                wf <- loop wfname rproto  msg (level +1)
                return (option,wf)

      wfsoptions <- if s== "sel" then mapM defineBranch options else return []

      return $ \rsub  -> do
       group <- atomic $ readDBRef rgroup `onNothing` noRef rgroup
       elected <- WF.step $ do

          WF.exec ("askUser."++ wfname) wf rsub
          atomic $ do
             sub <-  readWFRef rsub `onNothing` noRef rsub
             selectOptions sub
       case s of
          "sel" -> do
            mapM_ (dispatch wfsoptions rproto) elected

            where
            dispatch ops ref op =
              case lookup op ops of
                 Nothing -> error $ "no Workflow defined for option: " ++ op
                 Just wf -> wf rsub


          queue ->  WF.step (push (getQRef queue) rsub)


spaces level= concat . take level $ repeat  "   "
--editVotInProto :: Int -> WFRef Subject -> FlowM Html IO ()
editVotInProto level rproto = do
   WFProto g c prot@Subject{..} <- atomic $  readDBRef rproto `onNothing` noRef rproto
   let votatio= reverse $ case prot of Subject{votation=vot} -> unsafeCoerce vot :: [Votation]
   vot <- askt (const $ votatio !!! level) $ edit $ votatio !!! level !> "editVotInProto"
   let (pre, post) = splitAt level votatio
   atomic . writeDBRef rproto $ WFProto g c  $
        Subject{ description = description
               , permissions = permissions
               , sfrom= sfrom, srecipient= srecipient
               , stimeout = stimeout
               , swfname  = swfname
               , votation =  reverse $ pre++(vot: tail post)
               , content  = content}



{-
history must visualize the entries and must be modificable
estructura:
  eleccion de proto
  grupo de autores
  loop
   edicion  de votacion
     añadir votación
   grupo de destinatarios que votan
      que hacer cuando ha salido opcion1.. loop
      que hacer cuando ha salido opcion2.. loop
-}

chooseProtos :: FlowM Html IO  WFProto
chooseProtos=  do
    prot <- atomic $ indexOf (\(WFProto _ _ s) -> description s)
    let (descs,rprotos)= unzip prot
    setHeader $ \html -> docTypeHtml << body << (h1 << "choose the proto" <> html)
    hist <- getAmendPage
    i <- askt (const 0) $   hist
             **>(getSelect . mconcat $ map (\(n,v)->setOption n (toHtml  v)) $ zip [0..] descs)
             <** submitButton "submit"

    atomic $  readDBRef (rprotos !!! i !!! 0) `onNothing` (error $ "not found: " ++ descs !!! i) !> "chooseprotos"

selectGroup :: String -> String -> View Html IO (DBRef Group)
selectGroup wfname text  = do
   user <- getCurrentUser
   setHeader $ \html ->  docTypeHtml <<  body << html
   gnames <- atomic $ Q.select pname $ pauthors `containsElem` user
   hist <- getAmendPage

   (hist**> (if not $ null gnames
       then
           p << b << ("select the group for "<> text)
       ++> p << b << "choose a group"
       ++> getRadio [\n -> setRadioActive g n <++ El.span << g | g<- gnames]
           `waction` \n-> return $ getDBRef $ key uGroup{pname=n} !> n
       else noWidget)
       <|>  wlink () (p << b << "create a new group" )
               `waction` (const . askt (error "creategroup") $ creategroup user gnames))

   `waction` \rgr -> atomic $ do
        gr <- readDBRef rgr `onNothing` noRef rgr
        let gr'=  gr -- {pcategories= ("askUsers."++ wfname):pcategories gr}
        writeDBRef rgr gr'

        return rgr
    where
    creategroup user gnames= do
           gr <- (create uGroup{pname= "group x "++wfname++" created by "++ user
                                     ,pusers= M.singleton user author}
                  <** submitButton "submit")
                 `validate` (\gr -> case pname gr `elem`gnames of
                                      True -> return . Just . fromStr $ "use another name for the group: "++ pname gr
                                      False-> return Nothing)
           atomic $ newDBRef  gr


getInternalWF
  :: String ->  DBRef Group ->  FlowM Html WIO (WFRef Subject ->  WIO ())
getInternalWF wfname rgroup =  do
  let askwfname= "askUsers."++wfname

  (flag, flag2,keyGroup,user,gname)<- step $ do
      user  <- getCurrentUser
      group <- atomic $ readDBRef rgroup `onNothing` noRef rgroup
      return $(askwfname `elem`  pcategories group
             , PAuthor user `elem` pauthors group
             , key group
             , user
             , pname group)
  if flag
     then getConfigureWF' askwfname "void" >>= return . snd
     else if flag2
            then return $ \rsub -> askGroup (getDBRef keyGroup) rsub
            else error $ "getInternalWF: "++user++ " is not an author of the group"++ gname


once= step
ever= liftIO

-- vote flow restart

type SubjectKey= String
newtype VoteFlows = VoteFlows [(WFName,SubjectKey)] deriving (Read, Show, Typeable)

voteflowsKey= "voteflows"

voteflows= getDBRef voteflowsKey

instance Indexable VoteFlows where key= const voteflowsKey

-- | add a list of flows to be scheduled. Each entry in the list is a pair @(path, flow)@
addVoteFlows wfs=  atomically $ do
   VoteFlows wfs' <- readDBRef voteflows `onNothing` return (VoteFlows [])
   writeDBRef voteflows . VoteFlows $ wfs ++ wfs'


restartVoteFlows= do
   VoteFlows vfs <- atomically $ readDBRef voteflows  `onNothing` return (VoteFlows [])
   wfs<- return . M.fromList =<< mapM makeWF vfs -- :: IO (WorkflowList IO Subject ())
   restartWorkflows wfs !> "restartWorkflows"
   where
   makeWF :: (WFName, SubjectKey) -> IO (String, Subject -> Workflow IO ())
   makeWF (w, k)= do
     (_,wf) <- runFlowConf $ getConfigureWF w k

     return (w, wf )

