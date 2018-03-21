{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Intray.Web.Server.Handler.Admin
    ( getAdminR
    , postAdminAccountDeleteR
    ) where

import Import

import Data.Time

import Yesod

import Intray.Client

import Intray.Web.Server.Foundation
import Intray.Web.Server.Time

getAdminR :: Handler Html
getAdminR =
    withAdminCreds $ \t -> do
        AdminStats {..} <- runClientOrErr $ clientAdminStats t
        users <- runClientOrErr $ clientAdminGetAccounts t
        now <- liftIO getCurrentTime
        token <- genToken
        withNavBar $(widgetFile "admin")

postAdminAccountDeleteR :: UserUUID -> Handler Html
postAdminAccountDeleteR uuid =
    withAdminCreds $ \t -> do
        NoContent <- runClientOrErr $ clientAdminDeleteAccount t uuid
        redirect AdminR

withAdminCreds :: (Token -> Handler Html) -> Handler Html
withAdminCreds func =
    withLogin $ \t -> do
        adminInfo <- runClientOrErr $ clientGetAccountInfo t
        if accountInfoAdmin adminInfo
            then func t
            else notFound
