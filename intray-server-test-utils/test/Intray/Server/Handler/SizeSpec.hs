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
    describe "GetSize" $
    it "does not count other accounts' items" $ \cenv ->
        forAllValid $ \t ->
            withValidNewUser cenv $ \t1 ->
                withValidNewUser cenv $ \t2 -> do
                    mr <-
                        runClientOrError cenv $ do
                            void $ clientPostAddItem t1 t
                            clientGetSize t2
                    mr `shouldBe` 0
