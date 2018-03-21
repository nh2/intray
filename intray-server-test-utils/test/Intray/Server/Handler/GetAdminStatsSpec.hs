{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Intray.Server.Handler.GetAdminStatsSpec
    ( spec
    ) where

import TestImport

import Intray.Client

import Intray.Client.Gen ()
import Intray.Data.Gen ()
import Intray.Server.TestUtils

spec :: Spec
spec =
    withIntrayServer $
    describe "get admin stats" $ do
        it "forbids non-admin users from fetching admin stats" $ \cenv ->
            requiresAdmin cenv clientAdminStats
        it "returns valid admin stats" $ \cenv ->
            withAdmin cenv $ \token -> do
                adminStats <- runClientOrError cenv $ clientAdminStats token
                shouldBeValid adminStats
