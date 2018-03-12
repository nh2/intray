{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Intray.Server.Handler.GetAdminStatsSpec
    ( spec
    ) where

import TestImport

import qualified Network.HTTP.Types as Http
import Servant.Client

import Intray.Client

import Intray.Client.Gen ()
import Intray.Data.Gen ()
import Intray.Server.TestUtils

spec :: Spec
spec =
    withIntrayServer $
    describe "get admin stats" $ do
        it "forbids non-admin users from fetching admin stats" $ \cenv ->
            withValidNewUser cenv $ \token -> do
                errOrStats <- runClient cenv $ clientAdminStats token
                case errOrStats of
                    Left err ->
                        case err of
                            FailureResponse {} ->
                                Http.statusCode (responseStatus err) `shouldBe`
                                401
                            _ ->
                                expectationFailure
                                    "Should have got a failure response."
                    Right _ ->
                        expectationFailure
                            "Should not have been allowed to view admin stats."
        it "returns valid admin stats" $ \cenv ->
            withAdmin cenv $ \token -> do
                adminStats <- runClientOrError cenv $ clientAdminStats token
                shouldBeValid adminStats
