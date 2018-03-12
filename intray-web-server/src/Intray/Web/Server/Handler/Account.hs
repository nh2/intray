{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}

module Intray.Web.Server.Handler.Account
    ( getAccountR
    ) where

import Import

import Yesod

import Intray.Client

import Intray.Web.Server.Foundation
import Intray.Web.Server.Time

getAccountR :: Handler Html
getAccountR =
    withLogin $ \t -> do
        AccountInfo {..} <- runClientOrErr $ clientGetAccountInfo t
        timestampWidget <- makeTimestampWidget accountInfoCreatedTimestamp
        withNavBar $(widgetFile "account")
