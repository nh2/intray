{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}

module Intray.Server.Handler.ShowItem where

import Import

import Database.Persist

import Servant hiding (BadPassword, NoSuchUser)
import Servant.Auth.Server as Auth
import Servant.Auth.Server.SetCookieOrphan ()

import Intray.API
import Intray.Data

import Intray.Server.Types

import Intray.Server.Handler.Utils
import Intray.Server.Item

serveShowItem ::
       AuthResult AuthCookie -> IntrayHandler (Maybe (ItemInfo TypedItem))
serveShowItem (Authenticated AuthCookie {..}) = do
    itemsEnt <-
        runDb $
        selectFirst
            [IntrayItemUserId ==. authCookieUserUuid]
            [Asc IntrayItemTimestamp]
    pure $ (makeItemInfo . entityVal) <$> itemsEnt
serveShowItem _ = throwAll err401
