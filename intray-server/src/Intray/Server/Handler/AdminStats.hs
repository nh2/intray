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
serveAdminStats (Authenticated AuthCookie {..}) = do
    admins <- asks envAdmins
    mUser <- runDb $ getBy $ UniqueUserIdentifier authCookieUserUuid
    case mUser of
        Nothing -> throwError $ err404 {errBody = "User not found."}
        Just (Entity _ User {..}) ->
            if userUsername `elem` admins
                then do
                    adminStatsNbUsers <- runDb $ count ([] :: [Filter User])
                    adminStatsNbItems <-
                        runDb $ count ([] :: [Filter IntrayItem])
                    pure AdminStats {..}
                else throwAll err401
serveAdminStats _ = throwAll err401
