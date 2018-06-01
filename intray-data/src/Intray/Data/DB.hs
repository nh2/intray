{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Intray.Data.DB where

import Import

import Data.Time

import Database.Persist.Sql
import Database.Persist.TH

import Intray.Data.AccountUUID
import Intray.Data.HashedPassword
import Intray.Data.ItemType
import Intray.Data.ItemUUID
import Intray.Data.Username

share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    [persistLowerCase|

User
    identifier AccountUUID
    username Username
    hashedPassword HashedPassword
    createdTimestamp UTCTime
    lastLogin UTCTime Maybe
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
