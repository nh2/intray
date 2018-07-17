{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Intray.API
    ( IntrayAPI
    , intrayAPI
    , IntraySite(..)
    , IntrayOpenAPI
    , intrayOpenAPI
    , IntrayOpenSite(..)
    , IntrayProtectedAPI
    , IntrayProtectedSite(..)
    , IntrayProtectedItemAPI
    , IntrayProtectedItemSite(..)
    , IntrayProtectedAccountAPI
    , IntrayProtectedAccountSite(..)
    , IntrayProtectedAccessKeyAPI
    , IntrayProtectedAccessKeySite(..)
    , IntrayPublicAPI
    , IntrayPublicSite(..)
    , IntrayAdminAPI
    , IntrayAdminSite(..)
    , AuthCookie(..)
    , Permission(..)
    , userPermissions
    , adminPermissions
    , ItemType(..)
    , TypedItem(..)
    , textTypedItem
    , TypedItemCase(..)
    , typedItemCase
    , ItemInfo(..)
    , Added(..)
    , Synced(..)
    , SyncRequest(..)
    , SyncResponse(..)
    , AccountInfo(..)
    , AccessKeyInfo(..)
    , AddAccessKey(..)
    , AccessKeyCreated(..)
    , accessKeySecretText
    , DeleteAccessKey
    , Registration(..)
    , PostRegister
    , LoginForm(..)
    , PostLogin
    , GetDocs
    , GetDocsResponse(..)
    , AdminStats(..)
    , AdminGetStats
    , AdminDeleteAccount
    , AdminGetAccounts
    , HashedPassword
    , passwordHash
    , validatePassword
    , ItemUUID
    , AccountUUID
    , AccessKeyUUID
    , Username
    , parseUsername
    , parseUsernameWithError
    , usernameText
    , module Data.UUID.Typed
    ) where

import Import

import Data.UUID.Typed

import Web.Cookie

import Servant.API
import Servant.Auth.Docs ()
import Servant.Auth.Server.SetCookieOrphan ()
import Servant.API.Generic
import Servant.HTML.Blaze

import Intray.Data

import Intray.API.Admin
import Intray.API.Protected
import Intray.API.Types

intrayAPI :: Proxy IntrayAPI
intrayAPI = Proxy

type IntrayAPI = ToServantApi IntraySite

data IntraySite route = IntraySite
    { openSite :: route :- ToServantApi IntrayOpenSite
    , adminSite :: route :- "admin" :> ToServantApi IntrayAdminSite
    } deriving (Generic)

intrayOpenAPI :: Proxy IntrayOpenAPI
intrayOpenAPI = Proxy

type IntrayOpenAPI = ToServantApi IntrayOpenSite

data IntrayOpenSite route = IntrayOpenSite
    { protectedSite :: route :- ToServantApi IntrayProtectedSite
    , publicSite :: route :- ToServantApi IntrayPublicSite
    } deriving (Generic)

type IntrayPublicAPI = ToServantApi IntrayPublicSite

data IntrayPublicSite route = IntrayPublicSite
    { postRegister :: route :- PostRegister
    , postLogin :: route :- PostLogin
    , getDocs :: route :- GetDocs
    } deriving (Generic)

-- | The order of the items is not guaranteed to be the same for every call.
type PostRegister
     = "item" :> ReqBody '[ JSON] Registration :> Post '[ JSON] NoContent

type PostLogin
     = "login" :> ReqBody '[ JSON] LoginForm :> PostNoContent '[ JSON] (Headers '[ Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] NoContent)

type GetDocs = Get '[ HTML] GetDocsResponse
