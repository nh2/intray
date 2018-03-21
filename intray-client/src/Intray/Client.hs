{-# LANGUAGE DataKinds #-}

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
    , clientDeleteAccount
    , clientAdminStats
    , clientAdminDeleteAccount
    , clientAdminGetAccounts
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
    , Token
    , module Data.UUID.Typed
    ) where

import Import

import qualified Data.UUID.Typed
import Servant.API
import Servant.API.Flatten
import Servant.Auth.Client
import Servant.Auth.Server
import Servant.Auth.Server.SetCookieOrphan ()
import Servant.Client

import Intray.API

clientShowItem :: Token -> ClientM (Maybe (ItemInfo TypedItem))
clientSize :: Token -> ClientM Int
clientListItemUuids :: Token -> ClientM [ItemUUID]
clientListItems :: Token -> ClientM [ItemInfo TypedItem]
clientAddItem :: Token -> TypedItem -> ClientM ItemUUID
clientGetItem :: Token -> ItemUUID -> ClientM (ItemInfo TypedItem)
clientDeleteItem :: Token -> ItemUUID -> ClientM NoContent
clientSync :: Token -> SyncRequest -> ClientM SyncResponse
clientGetAccountInfo :: Token -> ClientM AccountInfo
clientDeleteAccount :: Token -> ClientM NoContent
clientRegister :: Registration -> ClientM NoContent
clientLogin ::
       LoginForm
    -> ClientM (Headers '[ Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] NoContent)
clientDocs :: ClientM GetDocsResponse
clientAdminStats :: Token -> ClientM AdminStats
clientAdminDeleteAccount :: Token -> UserUUID -> ClientM NoContent
clientAdminGetAccounts :: Token -> ClientM [AccountInfo]
clientShowItem :<|> clientSize :<|> clientListItemUuids :<|> clientListItems :<|> clientAddItem :<|> clientGetItem :<|> clientDeleteItem :<|> clientSync :<|> clientGetAccountInfo :<|> clientDeleteAccount :<|> clientRegister :<|> clientLogin :<|> clientDocs :<|> clientAdminStats :<|> clientAdminDeleteAccount :<|> clientAdminGetAccounts =
    client (flatten intrayAPI)
