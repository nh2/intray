{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}

module Intray.Server.Handler.AdminDeleteAccount
    ( serveAdminDeleteAccount
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

serveAdminDeleteAccount ::
       AuthResult AuthCookie -> UserUUID -> IntrayHandler NoContent
serveAdminDeleteAccount (Authenticated AuthCookie {..}) uuid =
    withAdminCreds authCookieUserUuid $ do
        mEnt <- runDb $ getBy $ UniqueUserIdentifier uuid
        case mEnt of
            Nothing -> throwError $ err404 {errBody = "User not found."}
            Just (Entity uid _) -> runDb $ delete uid
        pure NoContent
serveAdminDeleteAccount _ _ = throwAll err401
