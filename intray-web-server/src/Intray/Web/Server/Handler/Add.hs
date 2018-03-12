{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Intray.Web.Server.Handler.Add
    ( getAddR
    ) where

import Import

import Yesod

import Intray.Web.Server.Foundation

getAddR :: Handler Html
getAddR =
    withLogin $ \_ -> do
        token <- genToken
        withNavBar $(widgetFile "add")
