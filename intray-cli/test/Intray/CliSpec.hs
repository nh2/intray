module Intray.CliSpec
    ( spec
    ) where

import TestImport

import Servant.Client

import Intray.Cli.TestUtils
import Intray.Server.TestUtils

spec :: Spec
spec =
    withIntrayServer $
    it "Going through the usual manual steps 'just works'" $ \(ClientEnv _ burl _) -> do
        intray
            [ "register"
            , "--username"
            , "testuser"
            , "--password"
            , "testpass"
            , "--url"
            , showBaseUrl burl
            , "--intray-dir"
            , "/tmp"
            ]
        intray
            [ "login"
            , "--username"
            , "testuser"
            , "--password"
            , "testpass"
            , "--url"
            , showBaseUrl burl
            , "--intray-dir"
            , "/tmp"
            ]
        intray
            [ "add"
            , "hello"
            , "world"
            , "--url"
            , showBaseUrl burl
            , "--intray-dir"
            , "/tmp"
            ]
        intray ["show", "--url", showBaseUrl burl, "--intray-dir", "/tmp"]
        intray ["done", "--url", showBaseUrl burl, "--intray-dir", "/tmp"]
        intray ["size", "--url", showBaseUrl burl, "--intray-dir", "/tmp"]
        intray ["sync", "--url", showBaseUrl burl, "--intray-dir", "/tmp"]
        intray ["logout", "--intray-dir", "/tmp"]
