{-# LANGUAGE TypeApplications #-}

module Intray.API.InstanceSpec
    ( spec
    ) where

import TestImport

import Test.Validity.Aeson

import Intray.API.Gen ()
import Intray.API.Types

spec :: Spec
spec = do
    eqSpec @Permission
    ordSpec @Permission
    genValidSpec @Permission
    jsonSpecOnValid @Permission
    eqSpec @Registration
    ordSpec @Registration
    genValidSpec @Registration
    jsonSpecOnValid @Registration
    eqSpec @LoginForm
    ordSpec @LoginForm
    genValidSpec @LoginForm
    jsonSpecOnValid @LoginForm
