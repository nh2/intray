{-# LANGUAGE TypeApplications #-}

module Intray.Client.StoreSpec
    ( spec
    ) where

import TestImport

import Intray.Client.Gen ()
import Intray.Data.Gen ()

import Intray.Client.Store

spec :: Spec
spec = do
    eqSpec @Store
    genValidSpec @Store
    jsonSpecOnValid @Store
    eqSpec @StoreItem
    genValidSpec @StoreItem
    jsonSpecOnValid @StoreItem
