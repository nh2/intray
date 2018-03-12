{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Intray.Cli.Commands.Add
    ( addItem
    ) where

import Import

import Data.Time

import Intray.API hiding (addItem)

import Intray.Cli.OptParse
import Intray.Cli.Store
import Intray.Cli.Sync

addItem :: Text -> CliM ()
addItem t = do
    now <- liftIO getCurrentTime
    modifyStoreAndSync $ addItemToStore (textTypedItem t) now
