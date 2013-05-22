{-# OPTIONS
             -XDeriveDataTypeable
             -XUndecidableInstances
             -XExistentialQuantification
             -XMultiParamTypeClasses
             -XTypeSynonymInstances
             -XFlexibleInstances
             -XScopedTypeVariables
             -XFunctionalDependencies
             -XFlexibleContexts
             -XRecordWildCards



#-}

module DataDefs where
import Data.TCache
import Data.TCache.IndexQuery
import Data.TCache.IndexText
import Data.TCache.Memoization
import Data.Persistent.Collection
import MFlow.Wai.Blaze.Html.All hiding (select,content,head,div)
import qualified Text.Blaze.Html5 as El(head,map,div)
import qualified Text.Blaze.Html5.Attributes as At(id)
import Data.String
import Data.RefSerialize hiding ((<|>))
import qualified Control.Workflow as WF
import qualified Control.Workflow.Patterns as Pat
import Data.TCache.DefaultPersistence
import qualified Data.Map as M
import Data.Typeable
import System.IO.Unsafe
import qualified Data.Vector as V
import Data.List
import Data.String
import Control.Concurrent.MVar
import Control.Concurrent
import Data.Maybe
import Data.Dynamic
import Data.Monoid
import Unsafe.Coerce
import Control.Monad.Trans
import Control.Monad.State
import Control.Monad.Trans.Maybe
import Control.Monad.STM
import Data.ByteString.Lazy.Char8(ByteString,pack, unpack)
import Control.Applicative
import Control.Monad
import Control.Exception
import Control.Workflow
import Data.Char
import System.Time
import Data.IORef

import qualified Control.Workflow.Configuration as Conf

import Debug.Trace
(!>)= flip trace






data DelegateType= Pr (DBRef Group)
                 | Sub SubjectName
                 | Topic TopicName
                 deriving (Read, Show, Eq, Ord)

type UserName= String

data Delegations= Delegations{
                 oUser           :: UserName
                ,delType         :: DelegateType
                ,delegatedFrom   :: [Int] 
                ,delegatedTo     :: Maybe UserName
                }deriving (Read,Show,Typeable)

instance Indexable Delegations where
   key del = "Deleg " ++ show (oUser del) ++ show (delType del)

data Priority= NoVoted | ByProject | ByTopic | BySubject | Direct deriving(Read,Show,Eq,Ord)

--getResponseQueue :: (Typeable a, Serialize a, Serialize [a]) => String -> RefQueue a
--getResponseQueue t= getQRef   (t ++ "Resp") 

getSendQueue :: (Typeable a, Serialize a, Serialize [a]) => String -> RefQueue a
getSendQueue = getQRef

askGroup ::  DBRef Group -> WF.WFRef  Subject -> Workflow IO ()
askGroup rgroup msg =
  let qsend= getQRef $ keyObjDBRef  rgroup !> "askGroup1"
  in do
   time <- WF.step $ do
      Subject{..} <-  atomically $ WF.readWFRef msg `onNothing` error "askGroup msg not found"
      push  qsend msg !> "askGroup2"
      let delta = getTimeout $ assert (not $ null votation) $ head votation
      return $  fromIntegral delta !> show delta

   flag <- getTimeoutFlag time
   WF.step . liftIO $ do
      sub <-  atomically $ readWFRef msg `onNothing` error ("askGroup not found Subject: "++ show msg)
      atomically  $        reachThreshold sub 
                 `orElse`  (waitUntilSTM flag >> return False)

      checkAproval sub


   return()
--   `finally1` (WF.unsafeIOtoWF $ deleteElem qsend msg)

   where
   reachThreshold :: Subject -> STM Bool
   reachThreshold sub= do
     thr <- threshold sub !> "reachThreshold"
     case thr of
       True -> return True
       _    -> retry


finally1 f end= f >> end `WF.catch`(\(e :: SomeException) -> end >> WF.throw e)


--   getResponseQueue1 (Left user)= return [getResponseQueue user]
--   getResponseQueue1 (Right rgroup)= do
--              mgroup <-  atomically $ readDBRef rgroup
--              return ( fmap ( map getResponseQueue . V.toList . pusers) mgroup)
--              `onNothing` return []

--

--
--askToUserTimeout
--  :: (Indexable a, Serialize a, Serialize [a], Typeable a, Indexable b, Serialize b, Serialize [b], Typeable b) =>
--     Integer -> DBRef User -> a -> Workflow IO (Maybe b)
--askToUserTimeout t user msg=
--  return .  listToMaybe =<< askQueueTimeout t user msg [user]



--
--askQueueTimeout
--  :: (Indexable a, Serialize a, Serialize [a], Typeable a, Indexable b, Serialize b, Serialize [b], Typeable b) =>
--     Integer -> DBRef x -> a -> [DBRef User]
--     -> Workflow IO [b]
--askQueueTimeout t user msg list =
--  let quser= getSendQueue   user
--  in do
--    WF.step $ push quser msg
--    let qrs = mapM getResponseQueue list
--    flag <- getTimeoutFlag t
--    WF.step . atomically $ (mapM popSTM qrs)
--                 `orElse` (
--                  do
--                   waitUntilSTM flag
--                   deleteElemSTM quser  msg
--                   return Nothing)
--    `CMC.catch` (unsafeIOtoWF . deleteIt quser)
--    where
--    deleteIt quser (err :: SomeException)= do
--     deleteElem quser  msg
--     Control.Exception.throw err

{- añadir roles a GroupUsers
metodo de calcular votos
 configuracion : rol- peso, defecto 1
 rol author, quitar authors
 wfconf para calcular votos:
    cuando  cond () - hacer
    tratar voto de calidad

como se editan los roles
map role user
map role weight
añadir role

hay que crear un widget para añadir elementos a una lista
  para roles
  para opciones de votación


-}
type Role= String
type GroupName   =  String
type TopicName   =  String
type SubjectName =  String
type GroupUsers    = M.Map  UserName Role
type GroupSubjects = [String]
type WFName= String
type Category = String
 
data Group= Group{
                 pname       :: GroupName
                ,pdescrip    :: String
                ,pusers      :: GroupUsers
                ,ptopics     :: [TopicName]
                ,psubjects   :: GroupSubjects
                ,ppublic     :: Bool
                ,pvisible    :: Bool
                ,pcategories :: [WFName]

              --  ,workflows :: [(WFName,Prototype)]
                }deriving (Read, Show, Typeable)

newtype PAuthor = PAuthor String deriving (Eq, Ord, Typeable)
untype (PAuthor s)= s
author= "author"
pauthors ::  Group -> [PAuthor]
pauthors = map (PAuthor . fst)  . filter (\(n,r)-> r== author) . M.assocs . pusers

type UserConf= Group

--getUserConf u= atomically $  newDBRef uGroup{pname=u
--                                       ,pauthor= u
--                                       ,pusers = V.singleton u
--                                       }

uGroup= Group (error1 "pname") "" ( M.fromList[]) [][] True True []
--data Prototype view = forall a.(Typeable a,Editable a view, Serialize a) => Prototype a

--instance Serialize (Prototype view) where
--  showp (Prototype x)= do
--        insertString . pack . show $ typeOf x
--        showp x
--
--  readp = do
--       typestr <- readp
--       case M.lookup typestr (unsafePerformIO $ readMVar list) of
--            Nothing ->  fail $ error $ "to reify IDynamic data in a polimorphic context, please call reifyType before data retrieval for data "++ typestr
--            Just  f ->  f
--       <?> subjectPrefix

data WFProto= WFProto{protGroupKey :: GroupName, protCategory:: Category, protSub::  Subject} deriving (Typeable)


instance Indexable WFProto where
   key WFProto{protGroupKey="",protCategory="",..}= description protSub
   key WFProto{..}= "proto-"++protCategory++  protGroupKey
   defPath = const $ defPath () ++ "protos/"

instance Show WFProto where
  show (WFProto g c  sub)= unpack $ runW $ do
       showp g
       showp c
       showp sub

instance Read WFProto where
  readsPrec _ str= [( runR readp1 $ pack str,"")]
   where
   readp1= do
      g <- readp
      c <- readp
      sub <- readp
      return $ WFProto g c sub

--data WFData = WFData{ wfname   :: WFName, FlowM Html IO (Workflow IO ())}
--              deriving(Typeable)
--instance (Typeable1 m , Typeable view)=> Typeable (WFData view m) where
--   typeOf = const $ mkTyConApp
--                    (mkTyCon "votes-0.0.1.DataDefs.WFData")
--                    [typeOf (undefined :: view), typeOf1 (undefined :: m view)]

--registerWorkflow :: WFData  -> IO ()
--registerWorkflow wfdata=
--  withResource  wfdata  doit
--  where
--  doit Nothing = wfdata
--  doit (Just wf) =
--    wf{wfgroups= nub $ wfgroups wf ++ wfgroups wfdata
--      ,wfprotos= wfprotos wf ++ wfprotos wfdata}

--createContent group= do
--    protos <- select wfprotos $ wfgroups `containsElem` groupPrefix ++ group



--instance  Serialize WFData  where
--  showp (WFData n grs prs)= do
--      showp n
--      showp grs
--      showp prs
--
--  readp = do
--      n <- readp
--      grs <- readp
--      prs <- readp
--      return $ WFData n grs prs
--
--instance Indexable WFData   where
--  key WFData{..}= "WFData-" ++ wfname





error1 s= error $ s ++ " undefined"

--addWorkflow  rgroup wfname docs= do
--   group@Group{..} <- readDBRef rgroup
--   writeDBRef rgroup group{workflows= workflows++ [(wfname, docs)]}


--groupRegister name desc author users =
--  atomicfirstOfy . newDBRef $ uGroup{pname= name,pdescrip= desc
--                                 ,pauthor= author, pusers= V.fromList users}

instance  Indexable Group where
   key g=  keyGroup $ pname g
   defPath = const $ defPath () ++  "group/"
groupPrefix= ""
keyGroup gn= groupPrefix ++ gn


--class FormLet a view => Editable a view | a -> view  where
--  etitle :: a -> String
--  etype  :: a -> String

class  Renderizable a m view   where
  create :: a -> View view m a
  create= edit
  render :: a -> View view m a
  vote   :: a -> View view m a
  vote= render
  edit   :: a -> View view m a
  edit= create


data Per= Create | ViewIt | Edit | Vote deriving (Read, Show, Eq)
type Perms= [Per]


--instance Monoid Perms where
--   p `mappend`  p'=   nub $ p ++ p'
--
--   mempty=  []


data Perm = Perm{userp, recip, allp :: Perms} deriving (Read, Show)

instance  Renderizable Perm IO Html where
   create = edit
   edit Perm{..} =
       table
          <<< thead << tr << (th <<  "" <> th <<  "create" <> th <<  "edit" <> th <<  "vote" <> th <<  "view")
          ++> (Perm
          <$> tr <<< (td <<  "user"  ++> perm1 userp )
          <*> tr <<< (td <<  "recipient group" ++> perm1 recip )
          <*> tr <<< (td <<  "other users"     ++> perm1 allp))
   render p = wrender p
   vote = render


(<++>) = (<>)
infixr 2 <++>
--
perm1 :: Perms -> View Html IO Perms
perm1 perms  =    map read <$> getCheckBoxes (
           td <<< getElem Create  (Create `elem` perms)
      <++> td <<< getElem Edit    (Edit `elem` perms)
      <++> td <<< getElem Vote    (Vote `elem` perms)
      <++> td <<< getElem ViewIt  (ViewIt `elem` perms))
  where

  getElem x  flag =   setCheckBox flag (show x)

class IsVotation b where
--  setKey ::  b  -> String-> IO b
  getStatus :: b -> Status
  setStatus :: b -> Status -> b
  threshold :: b -> STM Bool
  selectOptions :: b -> STM [String]
  getTimeout :: b -> Int
  setNElems :: Int -> b -> b
  getOptions :: b -> IO [String]
  checkAproval :: b -> IO()

--type Recipient= DBRef Group
instance IsVotation Subject where
--    setKey Subject{..} x= do
--       vot <- setKey  (head votation) x >>= \v -> return $ v  : tail votation
--       return Subject
--               { description = description
--               , permissions = permissions
--               , sfrom= sfrom, srecipient= srecipient
--               , stimeout = stimeout
--               , swfname  = swfname
--               , votation =  vot
--               , content  = content}
    getStatus Subject{..} = getStatus $ last votation
    setStatus Subject{..} x=
       Subject{  description = description
               , permissions = permissions
               , sfrom       = sfrom
               , srecipient  = srecipient
               , stimeout    = stimeout
               , swfname     = swfname
               , votation    = setStatus  (assert (not $ null votation) $ head votation) x : tail votation
               , content     = content}
    threshold Subject{..}    = threshold $ assert (not $ null votation) $ head votation        !> "threshold sub"
    selectOptions Subject{..} = selectOptions $ assert (not $ null votation) $ head votation
    getTimeout Subject{..}= getTimeout $ assert (not $ null votation) $ head votation
    getOptions Subject{..}= getOptions $ assert (not $ null votation) $ head votation
    setNElems n  Subject{..} =
       Subject{  description = description
               , permissions = permissions
               , sfrom= sfrom, srecipient= srecipient
               , stimeout = stimeout
               , swfname  = swfname
               , votation = setNElems n (assert (not $ null votation) $ head votation) : tail votation
               , content  = content}
    checkAproval Subject{..} = checkAproval $ head votation

addVotationElem vot sub@Subject{..}=
       if typeOf vot /= typeOf (assert (not $ null votation) $ head votation)  then error "type mismatch" else
       Subject{  description = description
               , permissions = permissions
               , sfrom= sfrom, srecipient= srecipient
               , stimeout = stimeout
               , swfname  = swfname
               , votation =  unsafeCoerce vot  :  votation
               , content  = content}

data Subject =
  forall a b.
       (Typeable a
       ,Indexable a
       ,Serialize a
       ,Renderizable a IO Html
       ,Typeable b
       ,Serialize b
       ,IsVotation b
--       ,Monoid b
       ,Renderizable b IO Html )

     => Subject{  description :: String
               , permissions :: Perm
               , sfrom, srecipient :: DBRef Group
               , stimeout :: Integer
               , swfname  :: String
               , content  :: a
               , votation :: [b]}
     deriving(Typeable)

--instance  Typeable Subject where
--   typeOf = const $ mkTyConApp
--                    (mkTyCon3 "votes-0.0.1" "Vote.DataDefs" "Subject")
--                    []



--instance Display view display => Editable (Subject view)  view where
--
--  etitle  Subject{..} = etitle content
--  etype   Subject{..} = etype content

--instance Renderizable Subject ByteString where
--  render  Subject{..} = render content -- error "Subject rendering not implemented"
--
instance Indexable Subject where
  key Subject{..}= keySubject  description

keySubject c= subjectPrefix ++ c
subjectPrefix= "Subject"

--createContent= getMultilineText "enter content"
--
--
--createVotation=
--      p <<< getSelect(
--               setOption "yesno" (fromString "yes/no") False <|>
--               setOption "createoptions" (fromString "Create Options") False
--                  <! [("onclick","document.getElementById('textf').style.visibility=visible")])
--      **> p << "enter the options separated by commas"
--      ++> (getMultilineText "" <! ([("id","textf")] ++ disableAttrs))
--    `wmodify` getCommaSep
--    `wmodify` getVotation
--    where
--    getVotation v (Just opts)= (v, ChooseOptions

disableAttrs = [("style","visibility:hidden")]


instance  Renderizable Subject IO Html where
  create Subject{..} = do
     user   <- getCurrentUser
     groups <- atomic $ select pname $ pusers `containsElem` user
     Subject
           <$> p << b <<  "Description" ++> getString (Just description)
--                liftIO . processEvent $ SetKey desc !> "SetKey1"
--                return desc
           <*> p << b <<  "Permissions" ++> create permissions
           <*> return (getDBRef $ keyGroup user)
           <*> p <<< (b <<  "Destination group" ++> (getDBRef  . keyGroup <$> getSelect  ( opts1  (groups :: [String]))))
           <*> return stimeout
           <*> return swfname
           <*> p << b <<  "Content"  ++> create content
           <*> if null votation
                then return []
                else p << b <<  "Votation" ++> (create (head votation) >>= \v -> return $ v:tail votation)


     where
     opts1 []   = noWidget
     opts1 [x]  = setOption1 x
     opts1 grps = mconcat (map setOption1 grps)

     setOption1 o= setOption o (fromStr o)

  edit sub@Subject {..} = do
     (desc,cont) <-
        (,) <$> p << b <<  "Description" ++> getString (Just description)
            <*> edit content
--     vot <- if not $ null votation
--             then  liftIO $ setKey (head votation)desc >>= \v -> return $ v : tail votation
--             else return  []
     return $ Subject
        {description= desc
        ,content=  cont
        ,votation= votation

        ,permissions= permissions
        ,sfrom= sfrom
        ,srecipient= srecipient
        ,swfname= swfname
        ,stimeout=stimeout
        }





  vote Subject{..}=
       Subject
           <$> return description
           <*> return permissions
           <*> return sfrom
           <*> return srecipient
           <*> return 0
           <*> return swfname
           <*> render content
           <*> (vote  (assert (not $ null votation) $ head votation) >>= \v -> return (v:tail votation))



  render Subject{..}=
      Subject
           <$> wrender description
           <*> wrender permissions
           <*> wrender sfrom
           <*> wrender srecipient
           <*> return 0
           <*> return swfname
           <*> render content
           <*> mapM render votation

--
--instance Indexable (DBRef a) where
--   key = keyObjDBRef




--instance (MonadIO m, Functor m, FormInput view)  =>  FormLet (VoteSub view) m view where
--  digest ( Just(VoteSub Subject {..}))  =



              -- <*> renderT content

--renderT x= noWidget `modify` ( const $ liftIO (render x) >>= \s -> return  ( [s] ,Just x) )







{-
instance  Serializable  (Subject )  where
   serialize = runW . showp
   deserialize= runR readp

-}
deserializeList ::  MVar (M.Map String  (STR Subject) )
deserializeList= unsafePerformIO $ newMVar $ M.fromList []

registerSubject
  :: (Renderizable b  IO Html,
      Renderizable a  IO Html,
      Serialize b,
      Serialize a,
      Indexable a,
--      Monoid b,
      Typeable b,
      IsVotation b,
      Typeable a) =>
      String -> a -> [b] -> IO Subject
registerSubject descript   a bs=
 let key= typesSubject a (assert (not $ null bs) $ head bs) in
 Conf.runConfiguration ("conf"++ key) $ do
   Conf.ever $ index (\(WFProto _ _ s) -> description s)

   let treadpx =  do
       symbol subjectPrefix
       desc  <- readp
       perms <- readpParens
       from  <- readpParens
       recipient <-  readpParens
       timeout <- readp
       swfname <-  readp
       content  <- readpParens `asTypeOf` return a
       votation <- readp `asTypeOf` return bs

       return Subject
                  {description= desc
                  ,permissions= perms -- (Perm [Create] [ViewIt] [])
                  ,sfrom=  from
                  ,srecipient= recipient
                  ,stimeout= timeout
                  ,swfname= swfname
                  ,content= content
                  ,votation= votation
                  }



   let sub= Subject {description= descript
                  ,permissions=(Perm [Create] [ViewIt,Vote] [])
                  ,sfrom= (getDBRef $ keyGroup anonymous)
                  ,srecipient=(getDBRef $ keyGroup anonymous)
                  ,stimeout= 0
                  ,swfname= ""
                  ,votation= bs
                  ,content= a}
--              `asTypeOf` subjectType f

   Conf.ever $ do
       modifyMVar_ deserializeList $ \l -> do
           case M.lookup key l of
             Just _ -> return l
             _      -> return $ M.insert key treadpx l



   Conf.once $ withResources [] $ \_ -> [WFProto "" "" sub]
   return sub

   where
       readpParens :: Serialize a => STR a
       readpParens = parens readp
--   subjectType :: (Subject  -> Workflow IO () ) ->  Subject
--   subjectType = error "subjectType undefined"



getQueueSubject :: String -> RefQueue (WF.WFRef  Subject )
getQueueSubject= getQRef

getQueueDrafts :: String -> RefQueue  Subject
getQueueDrafts= getQRef

typesSubject a b= showsPrec 0 (typeOf a) $ showsPrec 0 (typeOf  b) ""

instance  Serialize Subject  where
   showp s@(Subject{..})= do
       showp . typesSubject content $ assert (not $ null votation) head votation
       insertString $ pack subjectPrefix
       showp description
       showpParens  permissions
       showpParens  sfrom
       showpParens  srecipient
       showp  stimeout
       showp  swfname
       showpParens  content
       showp  votation

       where
       showpParens x= do
          insertString $ pack " ("
          showp x
          insertChar ')'

   readp = do
       typestr <- readp
       --let typestr= read typestr'
       let m = unsafePerformIO $  readMVar deserializeList
       case  m `seq`  M.lookup typestr m of
            Nothing ->  fail $ error $ "not registered Subject proto: "++ typestr
            Just  f ->  f
       <?> subjectPrefix

------------------

instance  Show Subject where
   show Subject{..}= subjectPrefix++" "++ (unpack . runW $ showp votation) ++ " "++  (unpack . runW $ showp content) ++ ")"




assignGroup ::  UserName -> Role -> DBRef Group ->  STM ()
assignGroup ru role rgr=  maybeError err $  do
    gr <- MaybeT $ readDBRef rgr
    lift $ writeDBRef rgr gr{pusers= M.insert ru role $ pusers gr }
    where
    err = errModule ++ "assignGroup: user not found: " ++ show ru

maybeError  err iox = runMaybeT iox >>= \x ->
  case x of
    Nothing -> error err
    Just x -> return x

errModule=  "Control.WorkFlow.Users: "



instance   (MonadIO m, Functor m,Executable m)
          => Renderizable Group m Html where
  create = edit

  edit gr@ Group{..}  = do
    user <- getCurrentUser
    prot <- atomic $ indexOf (\(WFProto _ _ s) -> description s)
    let (protos,_)= unzip prot

    users  <- atomic $ indexOf userName
    groups <- atomic $ indexOf (undefined :: Group -> GroupName)
    let usernames =  fst $ unzip users
        groupnames=  fst $ unzip groups
        findUsers u= return $ take 5 $ filter (isPrefixOf u) groupnames
                                ++ filter (isPrefixOf u) usernames

        selectUsers=
              p <<   "select user/group members"
                ++> (El.div ! At.id (fromString "groupUsers")
                <<< (M.fromList
                <$> wautocompleteEdit
                          "Select users and groups"
                          findUsers
                          userwidget
                          []))

    Group <$> getString (Just pname)
          <*> p <<   "description" ++> getMultilineText pdescrip
--          <*> return [user]
          <*> selectUsers
          <*> p <<  "select topics" ++> getTopics ptopics  --   :: [TopicName]
          <*> return []     -- :: GroupSubjects
          <*> p <<  "anyone can join? "
                ++> getBool True "public"  "not public" -- ,public    :: Bool
          <*> p <<  "is visible to anyone?"
                ++> getBool True "visible" "not visible" -- ,visible   :: Bool
          <*> return []
--          <*> p <<  "prototypes of proposals for which the group can create proposals ?" ++>
--              getCheckBoxes (mconcat(map (\proto -> setCheckBox False proto <++ (fromStr proto)) protos))
     where
     deleteFromGroup user= do
            liftIO $ withResources [gr] $ const $ [gr{pusers = M.delete   user pusers }]
            return .fromString $ "$('#" <>  user <> "').parent().remove()"

     userwidget mu= do
      del <- ajax  deleteFromGroup
      let member=case mu of
           Nothing -> mempty
           Just u ->
            case M.lookup u $ pusers  of
              Nothing -> mempty
              Just _ ->
                     b <<  "member " <>
                     a ! href (fromString "#")
--                       ! onclick (pack . del $"'"++u++"'")
                       ! At.id  (fromString u)
                       << ( "remove")

      let check1 u v= case v== u of
                           True -> [("checked","")]
                           False -> []

      (,) <$> wrender (fromMaybe ""  mu)
          <*> getRadio (map (\v n-> setRadio v  n  <! check1 n v) ["president","vocal","ordinary"])
          <++ member


  render Group{..} =
        Group
          <$> wrender pname
          <*> wrender pdescrip
--          <*> wrender pauthors
          <*> wrender pusers
          <*> wrender ptopics
          <*> wrender psubjects
          <*> wrender ppublic
          <*> wrender pvisible
          <*> wrender pcategories
--          <*> wrender pprotos

createCategories= error "createCategories: not implemented"

--getTopics :: (FormInput view, Functor m, Monad m)
--            => View view  m [TopicName]
getTopics ts=   getString (Just $ intercalate "," ts) `wmodify` getCommaSep

getCommaSep v ms =  return (v,fmap f ms)
      where

      f  s = case break comma s of
           (s',"") -> [s']
           (s',s'') -> s':f s''
      comma= (==',')

{-
-- basic votation
-- reducir yes/no chooseoptions a lo mismo: 2 o mas opciones

objetivo : dar posibilidad a mas de dos opciones
tiene sentido? mantengamonos en las dos optciones
como decir si ha sido exitoso o no?
puede definirse uns funcion unica?
numero opciones a elegir. approbal significa votos minimos.
    optionVotes :: [(String, Int, Status)],
como se decide si es positivo o negativo?

se pueden elegir multiples opciones y que el flujo haga cosas segun cada opcion
si el numero de opciones a elegir es uno, entonces, una rama de wf para cada caso
si son dos o mas lo general sería un filtro
filter :: optionVotes -> [wfname]
   y hacer getConfigure wfname
se podria poner como metodo de IsVotation
   pero como se define el filtro?
     preguntar cuantas opciones defecto 1
     preguntar que se hace si se elije cada opcion por separado
        cada wf puede ser de "configuración" o de decision
   hay que dar a elegir entre
     un wf unico para despues de la votacion -> geenralizar envio a una cola
     o un wf para cada opción.

como se elije si se corta la votacion antes de tiempo?
     preguntando "si se llega al limite de approb., finalizar <- parameter finishEarly

threshold desaparece. aparece un selectOptions :: optionVotes -> [String]
-}

data Why = Unconstitutional Percent | NegativeVote Percent | NotEnoughVotes Percent 
                                deriving (Read,Show, Eq)
type Percent= Float
data Status = Draft | Processing | Approbed Percent | 
              Rejected Why |Closed Status | Voted Status deriving (Read,Show,Eq)

data EventKey = EventKey String

instance Show EventKey where show _= ""
instance Read EventKey where readsPrec n s= [(undefined,s)]

data Votes= Votes{options :: [(String, Int,Status )]
                 ,votList :: M.Map UserName (Priority,Int)}
          | Vote1 EventKey UserName (Priority,Int)
          | AddOption EventKey String
          | ChangeStatus EventKey String Status
--          | SetKey String
                 deriving (Typeable,Read, Show)

instance Indexable Votes where
    key Votes{..}= assert False $ error "Votes has no key"
    key (Vote1 (EventKey k) uname _) =  k
    key (AddOption (EventKey k) s)=  k
    key (ChangeStatus (EventKey k) _ _)= k
--
--instance Show Votes where
--    show Votes{..}= "Votes "++show (map (\(v,_,s) -> (v,s)) options)
--    show (AddOption k s)= "Add "++ show s
--    show (Vote1 s u v)= show (u, v)
--    show (ChangeStatus k op st)= "ChangeStatus " ++ show op
----    show (SetKey k)= "Set "++show k
--
--instance Read Votes where
--  readsPrec n s= readsPrec1 n $ dropWhile isSpace s where
--    readsPrec1 n ('V':'o':'t':'e':'s':' ':s) =
--      let[(vs,r)]= readsPrec n s
--      in [(Votes ( map  (\(v,s) -> (v,0,s)) vs) M.empty,r)]
--
--    readsPrec1 n s@('(':_) =
--      let [((u,v),r)]= readsPrec n s
--      in [(Vote1 undefined u v,r)]
--
--    readsPrec1 n ('A':'d':'d':' ':s)=
--      let [(t,r)]= readsPrec n s
--      in [(AddOption undefined t,r)]
--
--    readsPrec1 n ('C':'h':'a':'n':'g':'e':'S':'t':'a':'t':'u':'s':s)=
--      let [(op ,r)]= readsPrec n s
--      in [(ChangeStatus  undefined n,r)]
--
--    readsPrec1 n s=
--      let [((u,v),r)]= readsPrec n s
--      in [(Vote1 undefined u v,r)]
--
--

instance Monoid Votes where
  mempty= Votes  []  M.empty
  mappend x (Votes  _ _)= x
  mappend vot@Votes{..} (AddOption _ s)=
       vot{options= options++[(s,0,Draft)]}

--  mappend vot@Votes{..} (SetKey k)=
--       vot{keyVotes= k}

  mappend vot@Votes{..} (Vote1 _ user (prior,noption))= trace (show options)$
       case M.lookup user votList !> ("vote "++ show noption) of
        Just (prior', substract) ->
         if prior < prior'
          then vot  !> "no sumado"
          else Votes{options= sum1 (-1) substract $ sum1 1 noption options ,
                 votList= M.insert user (prior, noption) votList } !> "sumado"

        Nothing -> Votes{options= sum1 1 noption options ,
                 votList= M.insert user (prior, noption) votList } !> "no encontrado"
       where
--       sum1 _ Nothing ops = ops
       sum1 inc  noption ops= map (\(i,(n,v,_)) ->
                  if i==noption
                    then (n, v+inc, Processing)
                    else (n, v, Processing))
                  $ zip [0..] $ ops

  mappend Votes{..} (ChangeStatus k op val)= Votes (map change options) votList
      where
      change (n,v,st) =
                  if n == op
                    then (n, v, val)
                    else (n, v, st)


  mappend x y= error $ "mappend "++ show x ++ " " ++ show y
{-
how to enforce sequencing?
 some events need to be sent syinchronously
reorder: group synchronous events
label them as synchronous
some operations may need to perform IO in the transaction? avoid it
How to synchronize workflows?
  stepNetwork espera eventos de red ademas de internos
  stepNetworkInteractive
     al inicio del WF lanza un evento sincrono para notificar que es
       el unico nodo que responderá a ese wf
       otros nodos respn con error
     o bien el nodo balanceador se asegura de ello.
     o bien todos los wf son stateless.

  que una maquina unica se encargue de ese usuario
     Asi aunque ese evento sea no commutativo, se puede manejar asincronamente
        porque se integran asincronamente, desde distintas maquinas,
        grupos de eventos sincronizados, procedentes de cada maquina
          eventos e1 de maquina1 sincronizados con eventos e2 de maquina2 sincronizados
          [e11,e12..e1n][e21,e22..e2n] ...
     problema similar cuando un usuario vota varias veces un mismo asunto
       El orden de llegada es importante
     el problema es como se asignan dinamicamente las maquinas-usuarios
        ask to the cloud:
          redirection: un nodo crea subnodos y maneja unas tablas de redirección
          que pueden ser sincronizadas sincronamente.
        cuando se añade un nodo y el usuario logea en ese nodo
          actualize la tabla de redireccion con un evento.

Como aprovechar el logging de eventos con timestamp
-}

data Votation= Votation{
    vstatus     :: Status,
    optionVotes :: EventId Votes,
    approbal    :: (Int, Float),
    necessary   :: (Int, Float),
    complaint   :: (Int, Float),
    noptions    :: Int,
    votationTime:: Int,
    finishEarly :: Bool,
    nelems      :: Int
    }
    deriving (Read, Show, Typeable)

votationProto= Votation Draft (EventId "") (0,50) (0,50) (0,10) 1 0 False 0




instance IsVotation Votation where
--  setKey vot k= do
--   votes <-  processEvent (SetKey k)
--   return vot
  getStatus = vstatus
  setStatus v x= v{vstatus=x}
  getTimeout = votationTime

  getOptions v = do
    (votes,_) <- atomically . getEventValue $ optionVotes v
    let opts =  options votes
    return $ map (\ (x,_,_)  ->x)  opts

  setNElems n vot= vot{nelems= n}

  threshold vot@Votation{..} = do
    selected <-  selectOptions vot !> "threshold Votation"
    case (length selected == noptions,finishEarly) of
             (True,True) -> return True
             _           -> return False

  selectOptions Votation{..} =
    case fst complaint of
       0  -> checkNecessary
       n' -> if n' < nelems then checkNecessary else return []
    where
    checkNecessary=
     case fst necessary of
       0  -> checkVotes
       n' -> if n' > nelems then checkVotes else return []


    checkVotes=do
     (votes,_) <- getEventValue optionVotes      !> "checkVotes"
     let opts  = options votes
         vots  = sortBy (\(_,x,_) (_,y,_) ->  y `compare` x) opts
         vots' = take noptions vots
     return . map (\(x,_,_)-> x) $ filter (\(n,v,_) -> v > fst approbal) vots'

  checkAproval Votation{..}= do
     (Votes{..}, _) <- atomically $ getEventValue optionVotes
     let vots  = sortBy (\(_,x,_) (_,y,_) ->  y `compare` x) options
     mapM_ check vots
     where
     EventId k = optionVotes
     check (op,vot,stat)=
          let percent= ((fromIntegral $vot * 100)/(fromIntegral  nelems ))
          in
           if (vot > fst approbal)
            then
              processEvent $ ChangeStatus  (EventKey k) op $ Approbed percent
            else
              processEvent . ChangeStatus  (EventKey k) op . Rejected $ NotEnoughVotes percent


(!!!) a b = (!!) a b !> "index"

instance  Renderizable Votation IO Html where
  render vot= wraw (fromStr $ show vot) >> return vot
  edit= create
  vote Votation{..}=do
    user <- getCurrentUser
    (vots,_) <- atomic $ getEventValue optionVotes
    let opts= let EventId votes= optionVotes  in options vots
        mchecked= fmap snd $ M.lookup user $ votList vots
        voted= case mchecked of
            Just i -> let(n,_,_)= options vots !!! i in n  !> "vote"
            Nothing -> ""
        attrVoted n= if n==voted then [("checked","true")] else []
        showpercent 0= "0%"

        showpercent num= assert (nelems >0 ) $
          let  v= show ((fromIntegral $ num * 100 !> (show $ nelems >0))/ (fromIntegral nelems)) ++ "%"
          in take (fromMaybe (length v) (findIndex (=='.') v) + 2) v
    Votation
        <$> return vstatus
        <*> do
            op <- table <<< caption <<  "Vote"
                  ++> thead << tr << (th << b <<  "Option"
                                  <>  th << b <<  "Votes"
                                  <>  th << b <<  "Percent"
                                  <>  th << b <<  "vote")
                  ++> (tbody
                     <<< getRadio [\n ->  tr <<< td << name
                                     ++>  td  << b  << show num
                                     ++>  td  << b  << showpercent num
                                     ++> (td <<< setRadio name n) <! attrVoted name
                                  |(name,num,_) <- opts])

            process user op

        <*> return approbal
        <*> return necessary
        <*> return complaint
        <*> return noptions
        <*> return votationTime
        <*> return finishEarly
        <*> return nelems
     where
     process user op = do
       let EventId k = optionVotes
       (vots,_) <- atomic $ getEventValue optionVotes
       let mnoption= findIndex (== op ) $ map(\(n,_,_) -> n !> show n) $ options vots
           noption = fromJust mnoption                         !> op
       liftIO . processEvent $ Vote1 (EventKey k) user (Direct,noption) !> show op
       return optionVotes

  create vot@Votation{..}=  do

    Votation
     <$> return vstatus

     <*> p << b <<  "select the options" ++> addLink ++>
         (wEditList El.div (\v -> El.div <<< getString v
                                         <++ delLink) ["Yes","No"] "add"
                                         >>= newVotation)

     <*>((,) <$> p << b <<  "select the number/percentaje for aprobal"
         ++> getInt1 (Just $ fst approbal )
                               <! [("id","appnumber")
                                  ,("oninput",intpercent "appnumber" "appercent")]

         <*> getFloat (Just $ snd approbal)
                               <! [("id","appercent")
                                  ,("oninput",percentint "appnumber" "appercent")])

     <*>((,) <$> p << b <<  "select the number/percentaje of votes necessary for a valid votation"
         ++> getInt1 (Just $ fst necessary )
                                <! [("id","neccnumber")
                                  ,("oninput",intpercent "neccnumber" "neccercent")]

         <*> getFloat (Just $ snd necessary )
                               <! [("id","neccercent")
                                  ,("oninput",percentint "neccnumber" "neccercent")])

     <*>((,) <$> p << b <<  "select the number/percentaje of complaints that would make the votation invalid"
         ++> getInt1 (Just $ fst complaint )
                               <! [("id","compnumber")
                                  ,("oninput",intpercent "compnumber" "compercent")]

         <*> getFloat (Just $ snd complaint )
                               <! [("id","compercent")
                                  ,("oninput",percentint "compnumber" "compercent")])

     <*> p << b << "select the number of options to choose in the votation among the available ones"
                ++> p << "in Yes/No votations, this must be one: either Yes or No"
                ++> getInt (Just 1)

     <*> p << b <<  "Duration of the votation process from the moment it is received"
         ++> (p <<< (timeout
                    <$>  b << "days" ++> getInt (Just 1) <![("size","5")]
                    <*>  b << "hours"++> getInt (Just 0) <![("size","5")]))

     <*> p << b <<  "Finish the votation as soon as the votes necessary have been reached?"
         ++> getBool False "true" "false"
     <*> return nelems

    where
    addLink= a ! href (fromString "#")
                 ! At.id (fromString "add")
                 <<  "add"
    delLink= a ! href (fromString "#")
                 ! onclick (fromString "this.parentNode.parentNode.removeChild(this.parentNode)")
                 <<  "delete"

    getInt1 v= getInt v  <![("size","5")]


    timeout  d h=  (d * 24 + h) * 60 *60

    percentint int percent= "document.getElementById('"++int++"').value="++ show nelems++"document.getElementById('"++percent++"').value/100"
    intpercent int percent= "document.getElementById('"++percent++"').value="++ show nelems++"*100/document.getElementById('"++int++"').value"

newVotation ops = do
     liftIO $ print ops
     event@(EventId k)  <- liftIO $ newEvent
     liftIO $ processEvents $ map (AddOption (EventKey k)) ops
     return event

getFloat x = getFractional x <! [("size","5")]

getFractional
  :: (Monad m, Functor m, Fractional a, Read a, Show a,FormInput view) =>
     Maybe a -> View view m a
getFractional v= read <$> (getString (fmap show v) )
               `validate` ( \s -> if and $ map(\d ->isDigit d || d=='.') s
                                    then return Nothing
                                    else return . Just $ fromStr "must be a decimal number")


atomic:: MonadIO m => STM a -> m a
atomic= liftIO . atomically


instance (IResource a, Typeable a
         ,Renderizable a m view,MonadIO m, Functor m)
         => Renderizable (DBRef a) m view where
   edit ref   = do
       r <- atomic (readDBRef ref) `onNothing` noRef ref
       r' <- edit r
       atomic  $ writeDBRef ref r'
       return ref

   create ref = do
       r <- atomic $ readDBRef ref `onNothing` noRef ref
       r' <- create r
       atomic $ writeDBRef ref r'
       return ref

   vote ref=  atomic (readDBRef ref `onNothing` noRef ref) >>=  vote >>= atomic . writeDBRef ref >> return ref
   render ref = atomic (readDBRef ref `onNothing` noRef ref) >>= render >> return ref


noRef ref= error $ "reference of type "++ show(typeOf ref)++" not found: "++ show ref


class Monoid t => HasInverse t where
  cancel :: t-> t

  -- v <> t <> w <> cancel t === v <> w


processReversibleEvents ops= do
       r<- mapM processReversibleEvent  ops
       return   $ last r

processReversibleEvent
    :: (Typeable a
    , Indexable a
    , Serialize a
    , HasInverse a
    , MonadIO m
    , MonadState (MFlowState v) m)
    =>  a -> FlowM v m a
processReversibleEvent t= do
   let k= key t
   (res,st) <- atomic . getEventValue $ EventId k
   is <- goingBack
   let (res',t')= if  is then  let t'= DataDefs.cancel t  in  (res <> t',t')
                         else (res <> t,t)
   logEvent st t'
   liftIO $ atomically $ writeCached k (const $ rebuild k) (res',st) 0
   breturn res'



processEvents ops= do
       r<- mapM processEvent  ops
       return   $ last r

processEvent
  :: (Typeable b, Indexable b, Serialize b, Monoid b)
  => b -> IO b
processEvent t = do
   let k= key t
   (res,st) <- atomically . getEventValue $ EventId k
   logEvent st t
   let res'=  res <> t
   atomically $ writeCached k (const $ rebuild k) (res',st) 0
   return res'

--processEventSTM
--  :: (Typeable b, Indexable b, Serialize b, Monoid b)
--  => b -> STM b
--processEventSTM t= do
--   let k= key t
--   (res,st) <- getEventValue k
--   unsafeIOToSTM $ logEvent st t -- should not retry and no duplicate write since
--   let res'=  res <> t
--   writeCached k (const $ rebuild k) (res',st) 0
--   return res'

logEvent st x= stepExec st $ return x

getEventValue :: (Serialize a,Typeable a, Monoid a) => EventId a -> STM (a, DBRef Stat)
getEventValue (EventId key)= cachedByKeySTM key 0 (spawn $ rebuild  key) !> "getEvent Value"

spawn p= do
  mv <- newEmptyMVar
  forkIO $  p >>= putMVar mv
  takeMVar mv

rebuild :: (Serialize a,Typeable a, Monoid a)=> String -> IO (a, DBRef Stat)
rebuild k = exec1nc k $ rebuild1 mempty
 where
 rebuild1 res= do
   is <- isInRecover
   if not is
     then do
        st <- getWFStat
        return (res,st) !> "rebuild"
     else do
        t <- WF.step $  undefined
        let res' = res <> t
        rebuild1 res'
{-
problema mempty que tenga key y cumpla las leyes de monoid
mempy <> x= x
x <> mempty= x
pero processEvent necesita primer evento
  primer evento Votes "" [] M.empty

ErrorEvent cuando una sequencia no estaba en teoria permitida.
   se puede ignorar
-}

data EventId a= EventId String deriving (Read, Show)

--instance Indexable a => Show (EventId a) where show (EventId x) = "EventId "++ show x
--instance (Typeable a, Indexable a, Serialize a, Monoid a)=> Read (EventId a) where
-- readsPrec n s= readsPrec1 n $ dropWhile isSpace s
--  where
--  readsPrec1 n  ('E':'v':'e':'n':'t':'I':'d':' ':r)=
--    let [(k,s1)]= readsPrec n r
--    in [(EventId  k,s1)]

newEvent= fmap EventId newFlow

