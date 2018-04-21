{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Intray.Server.Handler.Public.GetDocs
    ( serveGetDocs
    ) where

import Import

import Data.FileEmbed
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Text.Pandoc as Pandoc

import Servant hiding (BadPassword, NoSuchUser)
import Servant.Auth.Server as Auth
import Servant.Auth.Server.SetCookieOrphan ()
import Servant.Docs as Docs

import Intray.API

import Intray.Server.Types

serveGetDocs :: IntrayHandler GetDocsResponse
serveGetDocs =
    case intrayHtmlResponse of
        Left _ ->
            throwError $
            err500
                {errBody = "Failed to convert the docs from Markdown to HTML."}
        Right bs -> pure bs

intrayHtmlResponse :: Either String GetDocsResponse
intrayHtmlResponse =
    case Pandoc.readMarkdown def intrayDocs of
        Left err -> Left $ show err
        Right pandoc -> pure $ GetDocsResponse $ Pandoc.writeHtml def pandoc

intrayDocs :: String
intrayDocs =
    Docs.markdown $ Docs.docsWithIntros [intr] $ Docs.pretty intrayOpenAPI
  where
    intr =
        Docs.DocIntro
            "Intray API"
            [ unlines
                  [ "<style>"
                  , T.unpack $ TE.decodeUtf8 $(embedFile "res/style/docs.css")
                  , "</style>"
                  ]
            ]
