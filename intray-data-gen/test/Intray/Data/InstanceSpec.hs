{-# LANGUAGE TypeApplications #-}

module Intray.Data.InstanceSpec
    ( spec
    ) where

import TestImport

import Test.Validity.Aeson

import Intray.Data.Gen ()

import Intray.API
import Intray.Data

spec :: Spec
spec = do
    eqSpec @ItemUUID
    ordSpec @ItemUUID
    jsonSpecOnValid @ItemUUID
    genValidSpec @ItemUUID
    eqSpec @ItemType
    ordSpec @ItemType
    jsonSpecOnValid @ItemType
    genValidSpec @ItemType
    eqSpec @IntrayItem
    genValidSpec @IntrayItem
    eqSpec @(ItemInfo ByteString)
    genValidSpec @(ItemInfo ByteString)
    eqSpec @TypedItem
    ordSpec @TypedItem
    genValidSpec @TypedItem
    jsonSpecOnValid @TypedItem
    eqSpec @(ItemInfo TypedItem)
    ordSpec @(ItemInfo TypedItem)
    genValidSpec @(ItemInfo TypedItem)
    jsonSpecOnValid @(ItemInfo TypedItem)
    eqSpec @NewSyncItem
    ordSpec @NewSyncItem
    genValidSpec @NewSyncItem
    jsonSpecOnValid @NewSyncItem
    eqSpec @SyncRequest
    ordSpec @SyncRequest
    genValidSpec @SyncRequest
    jsonSpecOnValid @SyncRequest
    eqSpec @SyncResponse
    ordSpec @SyncResponse
    genValidSpec @SyncResponse
    jsonSpecOnValid @SyncResponse
    eqSpec @Username
    ordSpec @Username
    genValidSpec @Username
    jsonSpecOnValid @Username
    eqSpec @HashedPassword
    eqSpec @Registration
    genValidSpec @Registration
    jsonSpecOnValid @Registration
    eqSpec @AccountUUID
    ordSpec @AccountUUID
    genValidSpec @AccountUUID
    jsonSpecOnValid @AccountUUID
