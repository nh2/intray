{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Intray.Server.Handler.Utils
    ( runDb
    , withAdminCreds
    ) where

import Import

import Database.Persist
import Database.Persist.Sqlite

import Servant
import Servant.Auth.Server as Auth

import Intray.API
import Intray.Data

import Intray.Server.Types

runDb :: (MonadReader IntrayServerEnv m, MonadIO m) => SqlPersistT IO b -> m b
runDb query = do
    pool <- asks envConnectionPool
    liftIO $ runSqlPool query pool

withAdminCreds :: UserUUID -> IntrayHandler a -> IntrayHandler a
withAdminCreds adminCandidate func = do
    admins <- asks envAdmins
    mUser <- runDb $ getBy $ UniqueUserIdentifier adminCandidate
    case mUser of
        Nothing -> throwError $ err404 {errBody = "User not found."}
        Just (Entity _ User {..}) ->
            if userUsername `elem` admins
                then func
                else throwAll err401
