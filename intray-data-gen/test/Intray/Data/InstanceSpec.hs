{-# LANGUAGE TypeApplications #-}

module Intray.Data.InstanceSpec
    ( spec
    ) where

import TestImport

import Test.Validity.Aeson

import Intray.Data

import Intray.Data.Gen ()

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
    eqSpec @Username
    ordSpec @Username
    genValidSpec @Username
    jsonSpecOnValid @Username
    eqSpec @HashedPassword
    eqSpec @AccountUUID
    ordSpec @AccountUUID
    genValidSpec @AccountUUID
    jsonSpecOnValid @AccountUUID
