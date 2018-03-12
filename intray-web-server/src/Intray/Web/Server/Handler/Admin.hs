{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Intray.Web.Server.Handler.Admin
    ( getAdminR
    ) where

import Import

import Yesod

import Intray.Client

import Intray.Web.Server.Foundation

getAdminR :: Handler Html
getAdminR =
    withLogin $ \t -> do
        AccountInfo {..} <- runClientOrErr $ clientGetAccountInfo t
        if accountInfoAdmin
            then do
                AdminStats {..} <- runClientOrErr $ clientAdminStats t
                withNavBar $(widgetFile "admin")
            else notFound
