{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Intray.Web.Server.Persistence
    ( loadLogins
    , storeLogins
    ) where

import Import

import Data.Aeson as JSON
import Data.Aeson.Encode.Pretty as JSON
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text.Encoding as TE

import Control.Concurrent

import Servant.Auth.Client (Token(..))

import Intray.Web.Server.Application ()
import Intray.Web.Server.Foundation

loginsFile :: IO (Path Abs File)
loginsFile = resolveFile' "logins.json"

loadLogins :: App -> IO ()
loadLogins app = do
    lf <- loginsFile
    mErrOrLogins <-
        forgivingAbsence $ JSON.eitherDecode <$> LB.readFile (toFilePath lf)
    modifyMVar_ (appLoginTokens app) $ \m ->
        case mErrOrLogins of
            Nothing -> pure m
            Just (Left err) -> do
                putStrLn $
                    unwords
                        [ "Failed to load logins from"
                        , fromAbsFile lf
                        , "with error:"
                        , err
                        ]
                pure m
            Just (Right r) -> pure r

storeLogins :: App -> IO ()
storeLogins app = do
    lf <- loginsFile
    m <- readMVar (appLoginTokens app)
    LB.writeFile (toFilePath lf) (JSON.encodePretty m)

instance FromJSON Token where
    parseJSON =
        withText "Token" $ \t ->
            case Base16.decode $ TE.encodeUtf8 t of
                (h, "") -> pure $ Token h
                _ ->
                    fail
                        "Invalid token in JSON: could not decode from hex string"

instance ToJSON Token where
    toJSON (Token bs) =
        case TE.decodeUtf8' $ Base16.encode bs of
            Left _ ->
                error "Failed to decode hex string to text, should not happen."
            Right t -> JSON.String t
