{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Intray.Server.Handler.PostAddItemSpec
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
    describe "PostAddItem" $
    it "adds an item without crashing" $ \cenv ->
        forAllValid $ \t ->
            withValidNewUser cenv $ \token -> do
                uuid <- runClientOrError cenv $ clientPostAddItem token t
                shouldBeValid uuid
