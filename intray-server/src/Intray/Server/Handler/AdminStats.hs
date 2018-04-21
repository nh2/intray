{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}

module Intray.Server.Handler.AdminStats
    ( serveAdminStats
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

serveAdminStats :: AuthResult AuthCookie -> IntrayHandler AdminStats
serveAdminStats (Authenticated AuthCookie {..}) =
    withAdminCreds authCookieUserUUID $ do
        adminStatsNbUsers <- runDb $ count ([] :: [Filter User])
        adminStatsNbItems <- runDb $ count ([] :: [Filter IntrayItem])
        pure AdminStats {..}
serveAdminStats _ = throwAll err401
