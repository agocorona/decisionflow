-----------------------------------------------------------------------------
--
-- Module      :  Control.MeassageFlow.Interface
-- Copyright   :
-- License     :  BSD3
--
-- Maintainer  :  agocorona@gmail.com
-- Stability   :  experimental
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------
{-# OPTIONS
             -XCPP
             -XRecordWildCards
             -XScopedTypeVariables
             -XDeriveDataTypeable

 #-}

-- #define TEST

module Interface (editMessages

) where


import System.Time
import Control.Workflow
import DataDefs as UD

import Data.Persistent.Collection

import Control.Monad
import Control.Monad.Trans.Maybe
import Control.Monad.State
import Data.Monoid
import qualified Data.Map as M
import qualified Data.Vector as V
import Data.List
import Data.TCache
import Data.TCache.DefaultPersistence
import Data.TCache.IndexQuery
import Data.TCache.IndexText
import Data.Typeable
import Data.String
import Control.Concurrent.STM(atomically, STM)
import Control.Concurrent
import Control.Concurrent.MVar
import Data.ByteString.Lazy.Char8(ByteString,pack, append)


import Text.Printf
import MFlow.Wai.Blaze.Html.All
#ifdef TEST
  hiding (ask,askt,select,content,head,div)
import MFlow.Forms.Test
#endif

import qualified Text.Blaze.Html5 as El
import qualified Text.Blaze.Html5.Attributes as At

import Data.Dynamic

import Data.Maybe

import System.IO.Unsafe

import WFGenerator

ifNothing= flip fromMaybe


data Edits = EditSubject (Either (Int, String) String)
           | CreateSubject String String
           | Refresh
           | Exit
           | NewWF String
           deriving (Read, Show, Typeable)

#ifdef TEST
instance Generate Edits where
  generate= do
   i <- takeMVar genEditsCounter
   putMVar genEditsCounter (if i== 3 then 0 else i+1)
   case i of
     0 -> returns $ NewWF "aaa"
     1 -> returns $ CreateSubject "aaa" (keyGroup "test2")
     2 -> do
           let qdrafts = getQueueDrafts  "admin"
           (_,_,_,_,_,_,e):r <- getDrafts qdrafts
           returns  e
     3 -> do
           rgroups <- atomic $ pusers `containsElem` "admin"
           let qs = map (getQueueSubject . keyObjDBRef) rgroups
           (_,_,_,_,_,_,e):r <- getQueueElems qs rgroups "admin"
           returns e
   where
   returns x= return x !> show x
#endif

genEditsCounter= unsafePerformIO $ newMVar 0

getQueueElems qs rgroups username= do
  relemss <- liftIO $ mapM pickAll qs
  elems'  <- mapM (\relems -> atomic  (mapM (\r -> readWFRef r `onNothing` noRef r ) relems))
                   relemss
  let elems= zip3  [0..] elems' $ concat relemss
  return   [(description
             ,keyObjDBRef sfrom
             ,keyObjDBRef srecipient
             ,swfname
             ,show $ typeOf content
             ,show stimeout
             ,EditSubject $ Left(qn, key relem))
             |(qn,es, relem) <-  elems, Subject {..} <- es ]

getDrafts qdrafts = do
  ulems <- liftIO $ pickAll qdrafts
  return[(description
         ,keyObjDBRef sfrom
         ,keyObjDBRef srecipient
         ,swfname
         ,show $ typeOf content
         ,show stimeout
         ,EditSubject $ Right (key ulem))
         |ulem@Subject{..} <-  ulems ]

editMessages :: FlowM Html IO ()
editMessages= do
   username <- getCurrentUser -- <-  getUser Nothing userLogin
--
   rgroups <- atomic $ pusers `containsElem` username
   let qdrafts  = getQueueDrafts  username
   let qs = map (getQueueSubject . keyObjDBRef) rgroups
--
--   ulems <- liftIO $ pickAll qdrafts
--
--   relemss <- liftIO $ mapM pickAll qs
--   elems'  <- mapM (\relems -> liftIO $  return . catMaybes
--                   =<< atomically  (mapM readWFRef relems))
--                   relemss
--
--   let elems= zip3  [0..] elems' $ concat relemss
   qelems  <- getQueueElems qs rgroups username
   qdrelems <- getDrafts qdrafts
   let header= tr $ mconcat
                  [th  $ fs tname
                  ,th  $ fs tfrom
                  ,th  $ fs to
                  ,th  $ fs tsubject
                  ,th  $ fs ttype
                  ,th  $ fs texpires
                  ,th  $ fs todo]
       listSubjects :: View Html IO Edits
       listSubjects=
          table ! At.style (fs' "width: 100%; border: 1px solid #000;")
            <<< title  (b << tmainList)

            ++> tr  <<  th ! At.style (fs' "text-align:center")
                            ! colspan (fs' "7") << b <<  "Messages received"
            ++> header
            ++>
                (firstOf
                [tr  <<< td << a
                     ++> td << b'
                     ++> td << c
                     ++> td << d
                     ++> td << e
                     ++> td << f
                     ++>(td <<< wlink g << b <<  "view/edit/vote" )
                | (a,b',c,d,e,f,g)  <- qelems ]
            <|>
                 tr <<  th ! At.style (fs' "text-align:center")
                           ! colspan (fs' "7") << b <<  "Draft user messages"
            ++> header
            ++>
                firstOf
                [tr <<< td << h
                     ++> td << i
                     ++> td << j
                     ++> td << k
                     ++> td << l
                     ++> td << m
                     ++> (td <<< wlink n << b <<  "view/edit" )
--                             <|>  wlink (Submit  relem)(b << (" / submit" :: String)))))
                | (h,i,j,k,l,m,n)<- qdrelems])


--   clinks <- createLinks rgroups

   clearEnv
   r<-  ask $   listSubjects
            <|> createLinks rgroups
            <|> br ++> wlink Refresh << b <<  "Refresh"
            <|> br ++> wlink Exit << b << "exit"
            <++ br
            <|> b <<  "create new kind of task "
            ++> (NewWF <$> getString Nothing <! [("placeholder", "enter the name")]
            <** submitButton "start")
   liftIO $ print r
   case r !> show r of
     NewWF wfname -> do
       getConfigureWF' wfname "void"
       ask $ b <<  "task created" ++> wlink () << b <<   " press here" !> "asking"


     EditSubject lr -> do
       case lr of
        Left (qn, key) -> do
          (refx,sub) <- atomic $ do
                refx <- pickElemSTM (qs !!! qn) key  `onNothing` (error  $ "edit: not found: " ++ key)
                sub  <- readWFRef refx `onNothing` noRef refx
                return (refx,sub)

          sub' <- editit username key sub
          atomic $ writeWFRef refx (sub' :: Subject)


        Right  key -> do
          sub  <- liftIO $ pickElem qdrafts key `onNothing` (error $ "edit: not found: " ++ key)
          editit username key sub
          return() --          liftIO $ updateElem qdrafts (sub' :: Subject)

     CreateSubject wfname keyGr -> do
       let ref = getDBRef $ key (WFProto "void" wfname undefined)
       WFProto _ _ proto <- atomic $ readDBRef ref `onNothing` noRef ref
       uname <- getCurrentUser

       let sub' = proto{sfrom= getDBRef $ keyGroup uname ,swfname= wfname, srecipient= getDBRef keyGr}
#ifdef TEST
       ev <- newVotation ["yes","no"]
       let sub''= addVotationElem (votationProto{optionVotes=ev}) sub'
#else
       let sub''= sub'
#endif

       sub <- askt (const sub'') $ edit sub'
                  <++ br
                  <** submitButton "create"


       -- donde se colocan los drafts? en la cola de usuario

       liftIO $ push qdrafts sub

     Refresh -> liftIO  syncCache
     Exit    -> liftIO $ syncCache >> myThreadId >>= killThread

   where
--   createLinks :: [DBRef Group] -> FlowM Html IO (View Html IO Edits)
   createLinks rgroups= do
     grs <- atomic $ mapM (\g -> readDBRef g `onNothing` error ("group no found" ++ show g)) rgroups
     firstOf [br ++> wlink (CreateSubject wfname key) << ("create "++ wfname ++ " for group "++key)
                      |(gr,key) <- zip grs (map key  grs),wfname <- pcategories gr]

   editit :: String -> String -> Subject -> FlowM Html IO Subject
   editit username  key1 sub@Subject{..} = do
       rgroups <- atomic $ pusers `containsElem` username
       let ver= verbs username sub rgroups

       (Just sub',Just op) <- askt (const (Just sub, Just "submit task"))
          $   p << ( b <<   ("category: " <> swfname))
          ++> if (Edit `elem` ver || Create `elem` ver) && getStatus sub== Draft then
                       edit sub
                   <++ br
                   <+> submitButton "save as draft"
                   <|> if keyGroup username == keyObjDBRef sfrom
                         then submitButton "submit task"
                         else noWidget

              else if  Vote `elem` ver then  vote sub <+> submitButton "vote"

              else  render sub <+> submitButton "OK"

       case op of
        "submit task" -> do
               (_,wf)  <- getConfigureWF swfname  (key sub')
--             let wf'= newWFRef (setStatus sub' Processing) >>= wf
--             liftIO . forkIO $  exec swfname  (const wf') (key sub') -- exec1nc (keyWF swfname  sub')  wf


               liftIO $ do
                   addVoteFlows [(swfname,key sub')] !> "addWorkflows"
                   forkIO $ exec swfname  wf sub' !> "exec"

               return()

        "save as draft" -> liftIO $ do
             let qdrafts = getQueueDrafts username
             if key sub== key sub'
                then updateElem qdrafts (sub' :: Subject)
                else push qdrafts sub'

        _ -> return ()
       return sub'


   fs= fromString
   fs'= fromString
   tmainList= "Subjects"
   tfrom= "from"
   tname= "Name"
   to= "to"
   tsubject= "Subject"
   ttype= "Type"
   texpires= "Expires"
   todo= "to do"

verbs ::  UserName -> Subject -> [DBRef Group] -> Perms
verbs user Subject{..} groups=
      let Perm perm1 perm2 perm3= permissions !> show permissions
          ruser= getDBRef $ keyGroup user
          isuser= if ruser == sfrom  then perm1 else mempty

          isgroup= if  ruser == srecipient  || srecipient `elem` groups
                        then perm2
                        else mempty

      in  perm3 `mappend` isuser `mappend` isgroup

