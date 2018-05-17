{-# LANGUAGE TypeApplications #-}

module Intray.API.Account.InstanceSpec
    ( spec
    ) where

import TestImport

import Test.Validity.Aeson

import Intray.API.Account.Gen ()
import Intray.API.Account.Types

spec :: Spec
spec = do
    eqSpec @AccountInfo
    ordSpec @AccountInfo
    genValidSpec @AccountInfo
    jsonSpecOnValid @AccountInfo
