module Intray.Server.Handler.ShowItemSpec
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
    describe "show item" $
    it "shows no item if the intray is empty, even if there are items in other accounts' intrays" $ \cenv ->
        forAllValid $ \t ->
            withValidNewUser cenv $ \t1 ->
                withValidNewUser cenv $ \t2 -> do
                    mr <-
                        runClientOrError cenv $ do
                            void $ clientAddItem t1 t
                            clientShowItem t2
                    mr `shouldBe` Nothing
