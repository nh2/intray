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
    genValidSpec @ItemUUID
    jsonSpecOnValid @ItemUUID
    eqSpec @ItemType
    ordSpec @ItemType
    genValidSpec @ItemType
    jsonSpecOnValid @ItemType
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
    eqSpec @Registration
    genValidSpec @Registration
    jsonSpecOnValid @Registration
    eqSpec @Username
    ordSpec @Username
    jsonSpecOnValid @Username
    genValidSpec @Username
    eqSpec @HashedPassword
    eqSpec @AccountUUID
    ordSpec @AccountUUID
    genValidSpec @AccountUUID
    jsonSpecOnValid @AccountUUID
