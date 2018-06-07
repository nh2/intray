{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}

module Intray.Server.Handler.PostSync
    ( servePostSync
    ) where

import Import

import Data.Mergeless
import qualified Data.Set as S
import Data.Time
import Data.UUID.Typed
import Database.Persist

import Servant hiding (BadPassword, NoSuchUser)
import Servant.Auth.Server as Auth
import Servant.Auth.Server.SetCookieOrphan ()

import Intray.API
import Intray.Data

import Intray.Server.Handler.Utils
import Intray.Server.Item
import Intray.Server.Types

servePostSync ::
       AuthResult AuthCookie
    -> SyncRequest ItemUUID TypedItem
    -> IntrayHandler (SyncResponse ItemUUID TypedItem)
servePostSync (Authenticated AuthCookie {..}) sr =
    withPermission authCookiePermissions PermitSync $ do
        now <- liftIO getCurrentTime
        let syncProcessorDeleteMany s =
                runDb $
                deleteWhere
                    [ IntrayItemUserId ==. authCookieUserUUID
                    , IntrayItemIdentifier <-. S.toList s
                    ]
            syncProcessorQuerySynced s =
                S.fromList . map (intrayItemIdentifier . entityVal) <$>
                runDb
                    (selectList
                         [ IntrayItemUserId ==. authCookieUserUUID
                         , IntrayItemIdentifier <-. S.toList s
                         ]
                         [])
            syncProcessorQueryNewRemote s =
                S.fromList . map (makeSynced . entityVal) <$>
                runDb
                    (selectList
                         [ IntrayItemUserId ==. authCookieUserUUID
                         , IntrayItemIdentifier /<-. S.toList s
                         ]
                         [])
            syncProcessorInsertMany s =
                runDb $
                insertMany_ $
                flip map (S.toList s) $ \Synced {..} ->
                    makeIntrayItem authCookieUserUUID syncedUuid now syncedValue
            proc = SyncProcessor {..}
        processSyncCustom nextRandomUUID now proc sr
servePostSync _ _ = throwAll err401
