{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}

module Intray.Web.Server.Handler.Account
    ( getAccountR
    , postAccountDeleteR
    ) where

import Import

import Yesod
import Yesod.Auth

import Intray.Client

import Intray.Web.Server.Foundation
import Intray.Web.Server.Time

getAccountR :: Handler Html
getAccountR =
    withLogin $ \t -> do
        AccountInfo {..} <- runClientOrErr $ clientGetAccountInfo t
        timestampWidget <- makeTimestampWidget accountInfoCreatedTimestamp
        token <- genToken
        withNavBar $(widgetFile "account")

postAccountDeleteR :: Handler Html
postAccountDeleteR =
    withLogin $ \t -> do
        NoContent <- runClientOrErr $ clientDeleteAccount t
        clearCreds False
        redirect HomeR
