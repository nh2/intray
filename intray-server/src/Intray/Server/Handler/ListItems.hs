{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}

module Intray.Server.Handler.ListItems
    ( serveListItems
    ) where

import Import

import Database.Persist

import Servant hiding (BadPassword, NoSuchUser)
import Servant.Auth.Server as Auth
import Servant.Auth.Server.SetCookieOrphan ()

import Intray.API
import Intray.Data

import Intray.Server.Types

import Intray.Server.Handler.Utils

serveListItems :: AuthResult AuthCookie -> IntrayHandler [ItemUUID]
serveListItems (Authenticated AuthCookie {..}) =
    fmap (fmap $ intrayItemIdentifier . entityVal) $
    runDb $
    selectList
        [IntrayItemUserId ==. authCookieUserUUID]
        [Asc IntrayItemTimestamp]
serveListItems _ = throwAll err401
