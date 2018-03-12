{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}

module Intray.Server.Handler.ListEntireIntray where

import Import

import Database.Persist

import Servant hiding (BadPassword, NoSuchUser)
import Servant.Auth.Server as Auth
import Servant.Auth.Server.SetCookieOrphan ()

import Intray.API
import Intray.Data

import Intray.Server.Item
import Intray.Server.Types

import Intray.Server.Handler.Utils

serveListEntireIntray ::
       AuthResult AuthCookie -> IntrayHandler [ItemInfo TypedItem]
serveListEntireIntray (Authenticated AuthCookie {..}) = do
    itemsEnts <-
        runDb $
        selectList
            [IntrayItemUserId ==. authCookieUserUuid]
            [Asc IntrayItemTimestamp]
    pure $ map (makeItemInfo . entityVal) itemsEnts
serveListEntireIntray _ = throwAll err401
