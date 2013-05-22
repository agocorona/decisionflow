-----------------------------------------------------------------------------
--
-- Module      :  Main
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
{-# OPTIONS  -XCPP
             -XMultiParamTypeClasses
             -XTypeSynonymInstances
             -XFlexibleInstances
             -XOverloadedStrings
             -XDeriveDataTypeable
             -XGeneralizedNewtypeDeriving
 #-}

-- #define TEST

module Main where
import MFlow.Wai.Blaze.Html.All
#ifdef TEST
  hiding (ask,userWidget)
import MFlow.Forms.Test
#endif
import qualified Text.Blaze.Html5 as El
import qualified Text.Blaze.Html5.Attributes as At
import MFlow.Forms.Admin
import DataDefs
import Interface
import WFGenerator

import Data.TCache
import Data.TCache.IndexQuery
import Data.TCache.IndexText
import Control.Concurrent
import Data.ByteString.Lazy.Char8(ByteString,pack)
import Data.Monoid
import Data.Vector as V(toList)
import Data.Text.Lazy as T(pack)
import Control.Monad
import Control.Monad.Trans
import qualified Data.Map as M
import qualified Control.Workflow as WF
import System.Time
import System.Directory

import Data.TCache.Defs
import Data.Typeable
import Control.Workflow
import Control.Workflow.Configuration

adminUser= "admin"

instance Renderizable String IO Html where
    render s= wraw (fromStr $ show s) >> return s
    edit str= p << b << text "Enter content"
              ++> getMultilineText str
              <! [("rows","10"),("cols","100")]

instance Monoid Int where mappend= (+); mempty= 0

newtype I= I Int deriving (Read, Show, Typeable,Monoid)

instance Indexable I where key _= "i"

removeData= do
   let fdata= ".tcachedata"
   exist <- doesDirectoryExist fdata

   when exist $ do
      putStrLn "deleting data .."
      removeDirectoryRecursive fdata


main= do

#ifdef TEST
   removeData
#endif

   syncWrite SyncManual
   index userName
   index pname
   indexList pusers (map T.pack  . M.keys)
   indexList pauthors (map (T.pack . untype))
   runConfiguration "conf"  $ do
       prot <- ever $ registerSubject "testsubject" ("content" ::String ) ([] :: [Votation])

       once $ do
         admin  <- setAdminUser adminUser adminUser
         userRegister "user1" "user1"
         userRegister "user2" "user2"
         userRegister "user3" "user3"

         atomically $ do
            newDBRef uGroup
                       {pname="test2"
                       ,pusers= M.fromList[(adminUser,author)
                                          ,("user2",author)
                                          ,("user3",author)]}


--      newDBRef uGroup
--                   {pname="test"
--                   ,pusers= M.fromList[(adminUser,author)
--                                      ,("user2",author)
--                                      ,("user3",author)]}

#ifdef TEST
   runTest1 . transient $ runFlowOnce mainFlow
   syncCache !> "1"

   runTest1 . transient $ runFlowOnce mainFlow
   syncCache !> "2"

   runTest1 . transient $ runFlowOnce mainFlow
   syncCache !> "3"

   runTest1 . transient $ runFlowOnce mainFlow
   syncCache !> "4"


#else
   restartVoteFlows

   addMessageFlows messageFlows
   wait $ run 80 waiMessageFlow

   where
   messageFlows=  [("", transient $ runFlow mainFlow)]
#endif
{-
como se generan distintos tipos de datos de contenido.
con runFlowIn?
    este genera funciones
    una funcion que es una estructura de datos?
       preguntar por nombres de datos de formulario
-}
htmlHeader html= docTypeHtml $
                 title << text "FreeChooser.com"
                 <> El.head << El.style << text
                            "table,th,td{border:1px solid black;}"
                 <> body << html

mainFlow :: FlowM Html IO ()
mainFlow= do
  liftIO $ print "mainFlow"
  setHeader htmlHeader  !> "setHeader"

--  login "admin"
  ask $ p << text "login as admin" ++> userWidget ( Just "admin") userLogin

  editMessages


