{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Intray.Server.Handler.SyncSpec
    ( spec
    ) where

import TestImport

import Intray.Client
import Intray.Client.Store

import Intray.Client.Gen ()
import Intray.Data.Gen ()
import Intray.Server.TestUtils

spec :: Spec
spec =
    withIntrayServer $
    describe "sync items" $ do
        it "produces a valid sync result for any sync request" $ \cenv ->
            forAllValid $ \syncRequest ->
                withValidNewUser cenv $ \token -> do
                    sr <- runClientOrError cenv $ clientSync token syncRequest
                    shouldBeValid sr
        it "is idempotent" $ \cenv ->
            forAllValid $ \initStore ->
                withValidNewUser cenv $ \token -> do
                    sr1 <-
                        runClientOrError cenv $
                        clientSync token $ makeSyncRequest initStore
                    let firstStore = mergeStore initStore sr1
                    sr2 <-
                        runClientOrError cenv $
                        clientSync token $ makeSyncRequest firstStore
                    let secondStore = mergeStore firstStore sr2
                    secondStore `shouldBe` firstStore
