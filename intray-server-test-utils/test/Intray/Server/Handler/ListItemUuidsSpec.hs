{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Intray.Server.Handler.ListItemUuidsSpec
    ( spec
    ) where

import TestImport

import Intray.Client

import Intray.Client.Gen ()
import Intray.Server.TestUtils

spec :: Spec
spec =
    withIntrayServer $
    describe "list items" $ do
        it "it lists item uuids of items that were just added" $ \cenv ->
            forAllValid $ \items ->
                withValidNewUser cenv $ \token -> do
                    uuids <-
                        runClientOrError cenv $ mapM (clientAddItem token) items
                    itemUuids' <-
                        runClientOrError cenv $ clientListItemUuids token
                    itemUuids' `shouldContain` uuids
        it "it always lists valid item uuids" $ \cenv ->
            withValidNewUser cenv $ \token -> do
                itemUuids <- runClientOrError cenv $ clientListItemUuids token
                shouldBeValid itemUuids
        it "does not list others' item uuids" $ \cenv ->
            forAllValid $ \items1 ->
                forAllValid $ \items2 ->
                    withValidNewUser cenv $ \token1 ->
                        withValidNewUser cenv $ \token2 -> do
                            uuids1 <-
                                runClientOrError cenv $
                                mapM (clientAddItem token1) items1
                            uuids2 <-
                                runClientOrError cenv $
                                mapM (clientAddItem token2) items2
                            itemUuids' <-
                                runClientOrError cenv $
                                clientListItemUuids token1
                            itemUuids' `shouldContain` uuids1
                            forM_ (uuids2 :: [ItemUUID]) $ \u ->
                                u `shouldNotSatisfy` (`elem` itemUuids')
