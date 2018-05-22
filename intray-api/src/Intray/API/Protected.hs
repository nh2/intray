{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Intray.API.Protected
    ( IntrayProtectedAPI
    , IntrayProtectedSite(..)
    , IntrayProtectedItemAPI
    , IntrayProtectedItemSite(..)
    , IntrayProtectedAccountAPI
    , IntrayProtectedAccountSite(..)
    , AuthCookie(..)
    , GetItemUUIDs
    , GetItems
    , GetShowItem
    , GetIntraySize
    , PostAddItem
    , GetItem
    , DeleteItem
    , ItemType(..)
    , TypedItem(..)
    , textTypedItem
    , TypedItemCase(..)
    , typedItemCase
    , ItemInfo(..)
    , SyncRequest(..)
    , NewSyncItem(..)
    , SyncResponse(..)
    , PostSync
    , AccountInfo(..)
    , GetAccountInfo
    , DeleteAccount
    , ItemUUID
    , AccountUUID
    , Username
    , parseUsername
    , parseUsernameWithError
    , usernameText
    ) where

import Import

import Servant.API
import Servant.Generic

import Intray.Data

import Intray.API.Protected.Account
import Intray.API.Protected.Item

type IntrayProtectedAPI = ToServant (IntrayProtectedSite AsApi)

data IntrayProtectedSite route = IntrayProtectedSite
    { protectedItemSite :: route :- "intray" :> ToServant (IntrayProtectedItemSite AsApi)
    , protectedAccountSite :: route :- "account" :> ToServant (IntrayProtectedAccountSite AsApi)
    } deriving (Generic)
