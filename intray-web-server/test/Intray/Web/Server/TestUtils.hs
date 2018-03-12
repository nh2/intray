{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Intray.Web.Server.TestUtils
    ( intrayTestServeSettings
    , intrayWebServerSpec
    , withExampleAccount_
    , withAdminAccount_
    ) where

import TestImport

import Yesod.Test

import Data.Text (Text)

import Network.HTTP.Types

import Database.Persist.Sqlite (mkSqliteConnectionInfo)

import Servant.Client (BaseUrl(..), ClientEnv(..))

import Intray.Data

import qualified Intray.Server.TestUtils as API

import Intray.Web.Server
import Intray.Web.Server.Foundation
import Intray.Web.Server.OptParse.Types

import Intray.Data.Gen ()

intrayTestServeSettings :: IO ServeSettings
intrayTestServeSettings = do
    let connInfo = mkSqliteConnectionInfo "test.db"
    pure
        ServeSettings
        { serveSetPort = 8000
        , serveSetPersistLogins = False
        , serveSetAPIPort = 8001
        , serveSetAPIConnectionInfo = connInfo
        , serveSetAPIConnectionCount = 4
        , serveSetAPIAdmins = [fromJust $ parseUsername "admin"]
        }

intrayWebServerSpec :: YesodSpec App -> Spec
intrayWebServerSpec = b . a
  where
    a :: YesodSpec App -> SpecWith ClientEnv
    a =
        yesodSpecWithSiteGenerator'
            (\(ClientEnv _ burl) -> do
                 sets <- intrayTestServeSettings
                 let sets' = sets {serveSetAPIPort = baseUrlPort burl}
                 makeIntrayApp sets')
    b :: SpecWith ClientEnv -> Spec
    b = API.withIntrayServer

withFreshAccount ::
       Username
    -> Text
    -> (Username -> YesodExample App a)
    -> YesodExample App a
withFreshAccount exampleUsername examplePassphrase func = do
    get $ AuthR registerR
    statusIs 200
    request $ do
        setMethod methodPost
        setUrl $ AuthR registerR
        addTokenFromCookie
        addPostParam "username" $ usernameText exampleUsername
        addPostParam "passphrase" examplePassphrase
        addPostParam "passphrase-confirm" examplePassphrase
    statusIs 303
    loc <- getLocation
    liftIO $ loc `shouldBe` Right AddR
    func exampleUsername

withExampleAccount :: (Username -> YesodExample App a) -> YesodExample App a
withExampleAccount =
    withFreshAccount (fromJust $ parseUsername "example") "pass"

withExampleAccount_ :: YesodExample App a -> YesodExample App a
withExampleAccount_ = withExampleAccount . const

withAdminAccount :: (Username -> YesodExample App a) -> YesodExample App a
withAdminAccount = withFreshAccount (fromJust $ parseUsername "admin") "admin"

withAdminAccount_ :: YesodExample App a -> YesodExample App a
withAdminAccount_ = withAdminAccount . const
