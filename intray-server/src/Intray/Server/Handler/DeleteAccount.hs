{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}

module Intray.Server.Handler.DeleteAccount
    ( serveDeleteAccount
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

serveDeleteAccount :: AuthResult AuthCookie -> IntrayHandler NoContent
serveDeleteAccount (Authenticated AuthCookie {..}) = do
    mEnt <- runDb $ getBy $ UniqueUserIdentifier authCookieUserUuid
    case mEnt of
        Nothing -> throwError $ err404 {errBody = "User not found."}
        Just (Entity uid _) -> runDb $ delete uid
    pure NoContent
serveDeleteAccount _ = throwAll err401
