{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Intray.Server.Handler.Utils
    ( runDb
    ) where

import Import

import Database.Persist.Sqlite

import Intray.Server.Types

runDb :: (MonadReader IntrayServerEnv m, MonadIO m) => SqlPersistT IO b -> m b
runDb query = do
    pool <- asks envConnectionPool
    liftIO $ runSqlPool query pool
