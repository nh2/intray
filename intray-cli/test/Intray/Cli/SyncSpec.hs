{-# LANGUAGE OverloadedStrings #-}

module Intray.Cli.SyncSpec
    ( spec
    ) where

import TestImport

import qualified Data.Text as T

import Servant.API
import Servant.Client

import Intray.Client
import Intray.Server.TestUtils

import Intray.Cli.LastSeen (readLastSeen)
import Intray.Cli.OptParse
import Intray.Cli.Session (loadToken)
import Intray.Cli.TestUtils

spec :: Spec
spec =
    withIntrayServer $
    it "correctly deletes the local LastSeen after a sync if the item has dissappeared remotely" $ \cenv ->
        forAllValid $ \ti ->
            withValidNewUserAndData cenv $ \un pw _ -> do
                let (ClientEnv _ burl _) = cenv
                let u = T.unpack $ usernameText un
                let p = T.unpack pw
                let d = "/tmp"
                dir <- resolveDir' d
                intray
                    [ "login"
                    , "--username"
                    , u
                    , "--password"
                    , p
                    , "--url"
                    , showBaseUrl burl
                    , "--intray-dir"
                    , d
                    ]
                let sets =
                        Settings
                        { setBaseUrl = Just burl
                        , setUsername = Just un
                        , setIntrayDir = dir
                        , setSyncStrategy = NeverSync
                        }
                mToken <- runReaderT loadToken sets
                token <-
                    case mToken of
                        Nothing -> do
                            expectationFailure
                                "Should have a token after logging in"
                            undefined
                        Just t -> pure t
                uuid <- runClientOrError cenv $ clientPostAddItem token ti
                intray ["sync", "--url", showBaseUrl burl, "--intray-dir", d]
                intray ["show", "--url", showBaseUrl burl, "--intray-dir", d]
                mLastSeen1 <- runReaderT readLastSeen sets
                mLastSeen1 `shouldSatisfy` isJust
                NoContent <- runClientOrError cenv $ clientDeleteItem token uuid
                intray ["sync", "--url", showBaseUrl burl, "--intray-dir", d]
                mLastSeen2 <- runReaderT readLastSeen sets
                mLastSeen2 `shouldSatisfy` isNothing
