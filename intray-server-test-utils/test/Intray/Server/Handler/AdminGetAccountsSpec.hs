{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Intray.Server.Handler.AdminGetAccountsSpec
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
    describe "AdminGetAccounts" $ do
        it "forbids non-admin users from getting account info" $ \cenv ->
            requiresAdmin cenv clientAdminGetAccounts
        it "only returns valid account info" $ \cenv ->
            withAdmin cenv $ \token -> do
                accountInfos <-
                    runClientOrError cenv $ clientAdminGetAccounts token
                shouldBeValid accountInfos
