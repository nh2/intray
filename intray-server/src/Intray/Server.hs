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

import Intray.Server.Handler.AdminDeleteAccount
import Intray.Server.Handler.AdminGetAccounts
import Intray.Server.Handler.AdminGetStats
import Intray.Server.Handler.DeleteAccount
import Intray.Server.Handler.DeleteItem
import Intray.Server.Handler.GetAccountInfo
import Intray.Server.Handler.GetDocs
import Intray.Server.Handler.GetIntraySize
import Intray.Server.Handler.GetItem
import Intray.Server.Handler.GetItemUUIDs
import Intray.Server.Handler.GetItems
import Intray.Server.Handler.GetShowItem
import Intray.Server.Handler.PostAddItem
import Intray.Server.Handler.PostLogin
import Intray.Server.Handler.PostRegister
import Intray.Server.Handler.PostSync

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
        { getShowItem = serveGetShowItem
        , getIntraySize = serveGetIntraySize
        , getItemUUIDs = serveGetItemUUIDs
        , getItems = serveGetItems
        , postAddItem = servePostAddItem
        , getItem = serveGetItem
        , deleteItem = serveDeleteItem
        , postSync = servePostSync
        , getAccountInfo = serveGetAccountInfo
        , deleteAccount = serveDeleteAccount
        }

intrayPublicServer :: IntrayPublicSite (AsServerT IntrayHandler)
intrayPublicServer =
    IntrayPublicSite
        { postRegister = servePostRegister
        , postLogin = servePostLogin
        , getDocs = serveGetDocs
        }

intrayAdminServer :: IntrayAdminSite (AsServerT IntrayHandler)
intrayAdminServer =
    IntrayAdminSite
        { adminGetStats = serveAdminGetStats
        , adminDeleteAccount = serveAdminDeleteAccount
        , adminGetAccounts = serveAdminGetAccounts
        }
