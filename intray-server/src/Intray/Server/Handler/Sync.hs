{-# LANGUAGE RecordWildCards #-}

module Intray.Server.Handler.Sync
    ( serveSync
    ) where

import Import

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

serveSync :: AuthResult AuthCookie -> SyncRequest -> IntrayHandler SyncResponse
serveSync (Authenticated AuthCookie {..}) SyncRequest {..} = do
    deleteUndeleted
    -- First we delete the items that were deleted locally but not yet remotely.
    -- Then we find the items that have been deleted remotely but not locally
    deletedRemotely <- syncItemsToBeDeletedLocally
    -- Then we find the items that have appeared remotely but aren't known locally
    newRemoteItems <- syncNewRemoteItems
    -- Then we add the items that should be added.
    newLocalItems <- syncAddedItems
    pure
        SyncResponse
            { syncResponseNewRemoteItems = newRemoteItems
            , syncResponseAddedItems = newLocalItems
            , syncResponseItemsToBeDeletedLocally = deletedRemotely
            }
  where
    deleteUndeleted :: IntrayHandler ()
    deleteUndeleted =
        runDb $
        deleteWhere
            [ IntrayItemUserId ==. authCookieUserUuid
            , IntrayItemIdentifier <-. syncRequestUndeletedItems
            ]
    syncItemsToBeDeletedLocally :: IntrayHandler [ItemUUID]
    syncItemsToBeDeletedLocally = do
        foundItems <-
            runDb $
            selectList
                [ IntrayItemUserId ==. authCookieUserUuid
                , IntrayItemIdentifier <-. syncRequestSyncedItems
                ]
                []
        -- 'foundItems' are the items that HAVEN'T been deleted
        -- So, the items that have been deleted are the ones in 'syncRequestSyncedItems' but not
        -- in 'foundItems'.
        pure $
            syncRequestSyncedItems \\
            map (intrayItemIdentifier . entityVal) foundItems
    syncNewRemoteItems :: IntrayHandler [ItemInfo TypedItem]
    syncNewRemoteItems =
        map (makeItemInfo . entityVal) <$>
        runDb
            (selectList
                 [ IntrayItemUserId ==. authCookieUserUuid
                 , IntrayItemIdentifier /<-. syncRequestSyncedItems
                 ]
                 [])
    syncAddedItems :: IntrayHandler [ItemInfo TypedItem]
    syncAddedItems = do
        now <- liftIO getCurrentTime
        forM syncRequestUnsyncedItems $ \NewSyncItem {..} -> do
            let ts = fromMaybe now newSyncItemTimestamp
            uuid <- liftIO nextRandomUUID
            runDb $
                insert_ $
                makeIntrayItem authCookieUserUuid uuid now newSyncItemContents
            pure
                ItemInfo
                    { itemInfoIdentifier = uuid
                    , itemInfoTimestamp = ts
                    , itemInfoContents = newSyncItemContents
                    }
serveSync _ _ = throwAll err401
