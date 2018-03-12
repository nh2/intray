{-# LANGUAGE OverloadedStrings #-}

module Intray.Web.Server.Handler.AuthRSpec where

import TestImport

import Network.HTTP.Types

import Yesod.Test

import Intray.Web.Server.Foundation
import Intray.Web.Server.TestUtils

{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}

spec :: Spec
spec =
    intrayWebServerSpec $
    ydescribe "RegisterR" $ do
        yit "gets a 200" $ do
            get $ AuthR registerR
            statusIs 200
        yit "registers an example account correctly" $ do
            registerFlow
            statusIs 303
            loc <- getLocation
            liftIO $ loc `shouldBe` Right AddR
            void followRedirect
            statusIs 200
        yit
            "fails to register and shows an error if an account with the same username exists" $ do
            registerFlow
            statusIs 303
            loc <- getLocation
            liftIO $ loc `shouldBe` Right (AuthR registerR)
            void followRedirect
            statusIs 200
            bodyContains "exists"

registerFlow :: YesodExample App ()
registerFlow = do
    let exampleUsername = "example"
    let examplePassphrase = "example"
    get $ AuthR registerR
    statusIs 200
    request $ do
        setMethod methodPost
        setUrl $ AuthR registerR
        addTokenFromCookie
        addPostParam "username" exampleUsername
        addPostParam "passphrase" examplePassphrase
        addPostParam "passphrase-confirm" examplePassphrase
