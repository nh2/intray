{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}

module Intray.Server.Handler.AccessKey.PostAddAccessKey
    ( servePostAddAccessKey
    ) where

import Import

import qualified Data.Set as S
import Data.Time

import Database.Persist

import Servant hiding (BadPassword, NoSuchUser)
import Servant.Auth.Server as Auth
import Servant.Auth.Server.SetCookieOrphan ()

import Intray.API
import Intray.Data

import Intray.Server.Types

import Intray.Server.Handler.Utils

servePostAddAccessKey ::
       AuthResult AuthCookie -> AddAccessKey -> IntrayHandler AccessKeyCreated
servePostAddAccessKey (Authenticated AuthCookie {..}) AddAccessKey {..} =
    withPermission authCookiePermissions PermitPostAddAccessKey $ do
        let perms = authCookiePermissions `S.difference` addAccesSKeyPermissions
        unless (perms == addAccesSKeyPermissions) $ throwAll err401
        uuid <- liftIO nextRandomUUID
        now <- liftIO getCurrentTime
        runDb $
            insert_
                AccessKey
                { accessKeyIdentifier = uuid
                , accessKeyUser = authCookieUserUUID
                , accessKeyHashedKey = undefined
                , accessKeyCreatedTimestamp = now
                , accessKeyPermissions = S.toList perms
                }
        secret <- liftIO generateRandomAccessKeySecret
        pure
            AccessKeyCreated
            { accessKeyCreatedCreatedTimestamp = now
            , accessKeyCreatedKey = secret
            , accessKeyCreatedUUID = uuid
            }
servePostAddAccessKey _ _ = throwAll err401
