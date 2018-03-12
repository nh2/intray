{-# LANGUAGE TemplateHaskell #-}

module Intray.Web.Server.Handler.Home
    ( getHomeR
    ) where

import Yesod

import Intray.Web.Server.Foundation

getHomeR :: Handler Html
getHomeR = withNavBar $(widgetFile "home")
