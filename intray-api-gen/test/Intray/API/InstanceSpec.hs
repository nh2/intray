{-# LANGUAGE TypeApplications #-}

module Intray.API.InstanceSpec
    ( spec
    ) where

import TestImport

import Test.Validity.Aeson

import Intray.API
import Intray.API.Gen ()

spec :: Spec
spec = do
    eqSpec @(ItemInfo ByteString)
    ordSpec @(ItemInfo ByteString)
    genValidSpec @(ItemInfo ByteString)
    eqSpec @TypedItem
    ordSpec @TypedItem
    genValidSpec @TypedItem
    jsonSpecOnValid @TypedItem
    jsonSpecOnValid @TypedItem
    eqSpec @(ItemInfo TypedItem)
    ordSpec @(ItemInfo TypedItem)
    genValidSpec @(ItemInfo TypedItem)
    jsonSpecOnValid @(ItemInfo TypedItem)
    eqSpec @SyncRequest
    ordSpec @SyncRequest
    genValidSpec @SyncRequest
    jsonSpecOnValid @SyncRequest
    eqSpec @NewSyncItem
    ordSpec @NewSyncItem
    genValidSpec @NewSyncItem
    jsonSpecOnValid @NewSyncItem
    eqSpec @SyncResponse
    ordSpec @SyncResponse
    genValidSpec @SyncResponse
    jsonSpecOnValid @SyncResponse
    eqSpec @AccountInfo
    ordSpec @AccountInfo
    genValidSpec @AccountInfo
    jsonSpecOnValid @AccountInfo
    eqSpec @Registration
    ordSpec @Registration
    genValidSpec @Registration
    jsonSpecOnValid @Registration
    eqSpec @LoginForm
    ordSpec @LoginForm
    genValidSpec @LoginForm
    jsonSpecOnValid @LoginForm
    eqSpec @AdminStats
    ordSpec @AdminStats
    genValidSpec @AdminStats
    jsonSpecOnValid @AdminStats
