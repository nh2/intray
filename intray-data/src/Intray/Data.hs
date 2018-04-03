{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}

module Intray.Data
    ( module Intray.Data
    , module X
    ) where

import Import

import Data.Time

import Database.Persist.Sql
import Database.Persist.TH

import Data.UUID.Typed as X
import Intray.Data.AccountUUID as X
import Intray.Data.HashedPassword as X
import Intray.Data.ItemType as X
import Intray.Data.ItemUUID as X
import Intray.Data.Username as X

share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    [persistLowerCase|

User
    identifier AccountUUID
    username Username
    hashedPassword HashedPassword
    createdTimestamp UTCTime
    UniqueUserIdentifier identifier
    UniqueUsername username
    deriving Show
    deriving Eq
    deriving Generic

IntrayItem
    identifier ItemUUID
    type ItemType
    contents ByteString
    timestamp UTCTime
    userId AccountUUID
    UniqueItem identifier type contents timestamp userId
    UniqueIdentifier identifier userId
    deriving Show
    deriving Eq
    deriving Generic
|]

instance Validity IntrayItem

instance Validity User
