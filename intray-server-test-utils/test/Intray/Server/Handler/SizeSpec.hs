module Intray.Server.Handler.SizeSpec
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
    describe "size" $
    it "does not count other accounts' items" $ \cenv ->
        forAllValid $ \t ->
            withValidNewUser cenv $ \t1 ->
                withValidNewUser cenv $ \t2 -> do
                    mr <-
                        runClientOrError cenv $ do
                            void $ clientAddItem t1 t
                            clientSize t2
                    mr `shouldBe` 0
