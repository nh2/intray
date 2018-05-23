{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Intray.Web.Server.Handler.AccessKeys
    ( getAccessKeysR
    , postAccessKeysR
    , postAccessKeyRevokeR
    ) where

import Import

import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text as T

import Yesod

import Intray.Client

import Intray.Web.Server.Foundation
import Intray.Web.Server.Time

getAccessKeysR :: Handler Html
getAccessKeysR =
    withLogin $ \t -> do
        accessKeys <- runClientOrErr $ clientGetAccessKeys t
        accessKeysWidgets <- mapM accessKeyWidget accessKeys
        permissions <- runClientOrErr $ clientGetPermissions t
        token <- genToken
        withNavBar $(widgetFile "access-keys")

newAccessKeyForm :: Set Permission -> FormInput Handler AddAccessKey
newAccessKeyForm ps =
    AddAccessKey <$> ireq textField "name" <*>
    (S.fromList . map fst . filter snd <$>
     traverse
         (\p -> (,) p <$> ireq checkBoxField (T.pack $ show p))
         (S.toList ps))

postAccessKeysR :: Handler Html
postAccessKeysR =
    withLogin $ \t -> do
        ps <- runClientOrErr $ clientGetPermissions t
        aac <- runInputPost $ newAccessKeyForm ps
        AccessKeyCreated {..} <- runClientOrErr $ clientPostAddAccessKey t aac
        timestampWidget <- makeTimestampWidget accessKeyCreatedCreatedTimestamp
        withNavBar $(widgetFile "access-key-created")

postAccessKeyRevokeR :: AccessKeyUUID -> Handler Html
postAccessKeyRevokeR uuid =
    withLogin $ \t -> do
        NoContent <- runClientOrErr $ clientDeleteAccessKey t uuid
        redirect AccessKeysR

accessKeyWidget :: AccessKeyInfo -> Handler Widget
accessKeyWidget AccessKeyInfo {..} = do
    token <- genToken
    timestampWidget <- makeTimestampWidget accessKeyInfoCreatedTimestamp
    pure $(widgetFile "access-key")
