{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Intray.API.Admin
    ( IntrayAdminAPI
    , IntrayAdminSite(..)
    , AdminStats(..)
    , AdminGetStats
    , AdminDeleteAccount
    , AdminGetAccounts
    ) where

import Import

import Servant.API
import Servant.Auth.Docs ()
import Servant.Auth.Server.SetCookieOrphan ()
import Servant.API.Generic

import Intray.Data

import Intray.API.Admin.Types
import Intray.API.Protected.Account.Types
import Intray.API.Types

type IntrayAdminAPI = ToServantApi IntrayAdminSite

data IntrayAdminSite route = IntrayAdminSite
    { adminGetStats :: route :- AdminGetStats
    , adminDeleteAccount :: route :- AdminDeleteAccount
    , adminGetAccounts :: route :- AdminGetAccounts
    } deriving (Generic)

type AdminGetStats = ProtectAPI :> "stats" :> Get '[ JSON] AdminStats

type AdminDeleteAccount
     = ProtectAPI :> "account" :> Capture "uuid" AccountUUID :> Delete '[ JSON] NoContent

type AdminGetAccounts = ProtectAPI :> "accounts" :> Get '[ JSON] [AccountInfo]
