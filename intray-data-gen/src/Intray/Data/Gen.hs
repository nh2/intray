{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Intray.Data.Gen where

import Import

import Data.GenValidity
import Data.GenValidity.ByteString ()
import Data.GenValidity.Text ()
import Data.GenValidity.Time ()
import Data.GenValidity.UUID ()
import qualified Data.Text as T

import Intray.API
import Intray.Data

instance GenUnchecked ItemUUID

instance GenValid ItemUUID

instance GenUnchecked AccountUUID

instance GenValid AccountUUID

instance GenUnchecked ItemType

instance GenValid ItemType

instance GenUnchecked IntrayItem

instance GenValid IntrayItem where
    genValid =
        IntrayItem <$> genValid <*> genValid <*> genValid <*> genValid <*>
        genValid

instance GenUnchecked TypedItem

instance GenValid TypedItem where
    genValid = (TypedItem <$> genValid <*> genValid) `suchThat` isValid

instance GenUnchecked a => GenUnchecked (ItemInfo a)

instance GenValid a => GenValid (ItemInfo a) where
    genValid =
        (ItemInfo <$> genValid <*> genValid <*> genValid) `suchThat` isValid

instance GenUnchecked Username

instance GenValid Username where
    genValid = do
        username <- parseUsername <$> textGen
        case username of
            Just name -> pure name
            Nothing -> genValid
      where
        textGen =
            T.pack <$>
            ((:) <$> charGen <*>
             ((:) <$> charGen <*> ((:) <$> charGen <*> genListOf charGen)))
        charGen = genValid `suchThat` validUsernameChar

instance GenUnchecked User

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

instance GenUnchecked HashedPassword

instance GenUnchecked AccountInfo

instance GenValid AccountInfo where
    genValid =
        (AccountInfo <$> genValid <*> genValid <*> genValid <*> genValid <*>
         genValid) `suchThat`
        isValid

instance GenUnchecked LoginForm

instance GenValid LoginForm where
    genValid = (LoginForm <$> genValid <*> genValid) `suchThat` isValid

instance GenUnchecked AdminStats

instance GenValid AdminStats where
    genValid = (AdminStats <$> genValid <*> genValid) `suchThat` isValid
