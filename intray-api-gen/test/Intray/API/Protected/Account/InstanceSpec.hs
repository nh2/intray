{-# LANGUAGE TypeApplications #-}

module Intray.API.Protected.Account.InstanceSpec
    ( spec
    ) where

import TestImport

import Test.Validity.Aeson

import Intray.API.Protected.Account.Gen ()
import Intray.API.Protected.Account.Types

spec :: Spec
spec = do
    eqSpec @AccountInfo
    ordSpec @AccountInfo
    genValidSpec @AccountInfo
    jsonSpecOnValid @AccountInfo
