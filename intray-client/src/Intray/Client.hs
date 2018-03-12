{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Intray.Client
    ( clientShowItem
    , clientSize
    , clientListItemUuids
    , clientListItems
    , clientAddItem
    , clientGetItem
    , clientDeleteItem
    , clientSync
    , clientRegister
    , clientLogin
    , clientDocs
    , clientGetAccountInfo
    , clientAdminStats
    , ItemType(..)
    , TypedItem(..)
    , textTypedItem
    , TypedItemCase(..)
    , typedItemCase
    , ItemInfo(..)
    , SyncRequest(..)
    , NewSyncItem(..)
    , SyncResponse(..)
    , AccountInfo(..)
    , Registration(..)
    , LoginForm(..)
    , GetDocsResponse(..)
    , AdminStats(..)
    , ItemUUID
    , UserUUID
    , Username
    , parseUsername
    , usernameText
    , NoContent(..)
    , module Data.UUID.Typed
    ) where

import Import

import qualified Data.UUID.Typed
import Servant.API
import Servant.Auth.Client
import Servant.Auth.Server
import Servant.Auth.Server.SetCookieOrphan ()
import Servant.Client

import Intray.API

intrayClient :: Client IntrayAPI
intrayClient = client intrayAPI

intrayOpenClient :: Client IntrayOpenAPI
intrayAdminClient :: Client IntrayAdminAPI
intrayOpenClient :<|> intrayAdminClient = intrayClient

intrayProtectedClient :: Client IntrayProtectedAPI
intrayPublicClient :: Client IntrayPublicAPI
intrayProtectedClient :<|> intrayPublicClient = intrayOpenClient

clientAdminStats :: Token -> ClientM AdminStats
clientAdminStats = intrayAdminClient

((clientShowItem :<|> clientSize) :<|> (clientListItemUuids :<|> clientListItems)) :<|> ((clientAddItem :<|> clientGetItem) :<|> (clientDeleteItem :<|> clientSync :<|> clientGetAccountInfo)) =
    intrayProtectedClient

clientRegister :<|> clientLogin :<|> clientDocs = intrayPublicClient

clientShowItem :: Token -> ClientM (Maybe (ItemInfo TypedItem))
clientSize :: Token -> ClientM Int
clientListItemUuids :: Token -> ClientM [ItemUUID]
clientListItems :: Token -> ClientM [ItemInfo TypedItem]
clientAddItem :: Token -> TypedItem -> ClientM ItemUUID
clientGetItem :: Token -> ItemUUID -> ClientM (ItemInfo TypedItem)
clientDeleteItem :: Token -> ItemUUID -> ClientM NoContent
clientSync :: Token -> SyncRequest -> ClientM SyncResponse
clientGetAccountInfo :: Token -> ClientM AccountInfo
clientRegister :: Registration -> ClientM NoContent
clientLogin ::
       LoginForm
    -> ClientM (Headers '[ Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] NoContent)
clientDocs :: ClientM GetDocsResponse
