{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-dodgy-exports #-}

module Intray.API.Gen
    ( module Intray.API.Gen
    , module Intray.API.Admin.Gen
    ) where

import Import

import Data.GenValidity
import Data.GenValidity.ByteString ()
import Data.GenValidity.Text ()
import Data.GenValidity.Time ()
import Data.GenValidity.UUID ()

import Intray.API
import Intray.Data.Gen ()

import Intray.API.Admin.Gen ()

instance GenUnchecked TypedItem

instance GenValid TypedItem where
    genValid = (TypedItem <$> genValid <*> genValid) `suchThat` isValid

instance GenUnchecked a => GenUnchecked (ItemInfo a)

instance GenValid a => GenValid (ItemInfo a) where
    genValid =
        (ItemInfo <$> genValid <*> genValid <*> genValid) `suchThat` isValid

instance GenUnchecked SyncRequest

instance GenValid SyncRequest where
    genValid =
        (SyncRequest <$> genValid <*> genValid <*> genValid) `suchThat` isValid

instance GenUnchecked NewSyncItem

instance GenValid NewSyncItem where
    genValid = (NewSyncItem <$> genValid <*> genValid) `suchThat` isValid

instance GenUnchecked SyncResponse

instance GenValid SyncResponse where
    genValid =
        (SyncResponse <$> genValid <*> genValid <*> genValid) `suchThat` isValid

instance GenUnchecked Registration

instance GenValid Registration where
    genValid = (Registration <$> genValid <*> genValid) `suchThat` isValid

instance GenUnchecked AccountInfo

instance GenValid AccountInfo where
    genValid =
        (AccountInfo <$> genValid <*> genValid <*> genValid <*> genValid <*>
         genValid) `suchThat`
        isValid

instance GenUnchecked LoginForm

instance GenValid LoginForm where
    genValid = (LoginForm <$> genValid <*> genValid) `suchThat` isValid
