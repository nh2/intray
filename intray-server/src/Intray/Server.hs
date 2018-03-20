{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}

module Intray.Server
    ( runIntrayServer
    , makeIntrayServer
    , intrayAppContext
    ) where

import Import

import Control.Monad.Logger
import Control.Monad.Trans.Resource (runResourceT)
import Database.Persist.Sqlite
import Network.Wai as Wai
import Network.Wai.Handler.Warp as Warp
import Network.Wai.Middleware.Cors

import Servant hiding (BadPassword, NoSuchUser)
import Servant.Auth.Server as Auth
import Servant.Auth.Server.SetCookieOrphan ()
import Servant.Generic

import Intray.API
import Intray.Data

import Intray.Server.OptParse.Types
import Intray.Server.Types

import Intray.Server.SigningKey

import Intray.Server.Handler.AddItem
import Intray.Server.Handler.AdminStats
import Intray.Server.Handler.DeleteAccount
import Intray.Server.Handler.DeleteItem
import Intray.Server.Handler.Docs
import Intray.Server.Handler.GetAccountInfo
import Intray.Server.Handler.GetItem
import Intray.Server.Handler.ListEntireIntray
import Intray.Server.Handler.ListItems
import Intray.Server.Handler.Login
import Intray.Server.Handler.Register
import Intray.Server.Handler.ShowItem
import Intray.Server.Handler.Size
import Intray.Server.Handler.Sync

runIntrayServer :: ServeSettings -> IO ()
runIntrayServer ServeSettings {..} =
    runStderrLoggingT $
    withSqlitePoolInfo serveSetConnectionInfo serveSetConnectionCount $ \pool -> do
        runResourceT $ flip runSqlPool pool $ runMigration migrateAll
        signingKey <- liftIO loadSigningKey
        let jwtCfg = defaultJWTSettings signingKey
        let cookieCfg = defaultCookieSettings
        let intrayEnv =
                IntrayServerEnv
                { envConnectionPool = pool
                , envCookieSettings = cookieCfg
                , envJWTSettings = jwtCfg
                , envAdmins = serveSetAdmins
                }
        liftIO $ Warp.run serveSetPort $ intrayApp intrayEnv

intrayApp :: IntrayServerEnv -> Wai.Application
intrayApp se =
    addPolicy . serveWithContext intrayAPI (intrayAppContext se) $
    makeIntrayServer se
  where
    addPolicy = cors (const $ Just policy)
    policy =
        simpleCorsResourcePolicy
        { corsRequestHeaders = ["content-type"]
        , corsMethods = ["GET", "POST", "HEAD", "DELETE"]
        }

intrayAppContext :: IntrayServerEnv -> Context '[ CookieSettings, JWTSettings]
intrayAppContext IntrayServerEnv {..} =
    envCookieSettings :. envJWTSettings :. EmptyContext

makeIntrayServer :: IntrayServerEnv -> Server IntrayAPI
makeIntrayServer cfg = enter (readerToEither cfg) (toServant intrayServer)
  where
    readerToEither :: IntrayServerEnv -> (IntrayHandler :~> Handler)
    readerToEither = runReaderTNat

intrayServer :: IntraySite (AsServerT IntrayHandler)
intrayServer =
    IntraySite
    { openSite = toServant intrayOpenServer
    , adminSite = toServant intrayAdminServer
    }

intrayOpenServer :: IntrayOpenSite (AsServerT IntrayHandler)
intrayOpenServer =
    IntrayOpenSite
    { protectedSite = toServant intrayProtectedServer
    , publicSite = toServant intrayPublicServer
    }

intrayProtectedServer :: IntrayProtectedSite (AsServerT IntrayHandler)
intrayProtectedServer =
    IntrayProtectedSite
    { showItem = serveShowItem
    , size = serveSize
    , listItems = serveListItems
    , listEntireIntray = serveListEntireIntray
    , addItem = serveAddItem
    , getItem = serveGetItem
    , deleteItem = serveDeleteItem
    , sync = serveSync
    , accountInfo = serveGetAccountInfo
    , deleteAccount = serveDeleteAccount
    }

intrayPublicServer :: IntrayPublicSite (AsServerT IntrayHandler)
intrayPublicServer =
    IntrayPublicSite
    {register = serveRegister, login = serveLogin, docs = serveDocs}

intrayAdminServer :: IntrayAdminSite (AsServerT IntrayHandler)
intrayAdminServer = IntrayAdminSite {adminStats = serveAdminStats}
