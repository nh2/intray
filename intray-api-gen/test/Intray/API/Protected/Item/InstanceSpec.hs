{-# LANGUAGE TypeApplications #-}

module Intray.API.Protected.Item.InstanceSpec
    ( spec
    ) where

import TestImport

import Test.Validity.Aeson

import Intray.API.Protected.Item.Gen ()
import Intray.API.Protected.Item.Types

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
