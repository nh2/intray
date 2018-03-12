{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Intray.Data.Username
    ( Username()
    , parseUsername
    , usernameText
    , validUsernameChar
    ) where

import Import

import Data.Aeson as JSON
import Data.Aeson.Types as JSON (toJSONKeyText)
import qualified Data.Char as Char
import Data.Hashable
import qualified Data.Text as T
import Database.Persist.Sql

newtype Username = Username
    { usernameText :: Text
    } deriving (Show, Eq, Ord, Generic)

instance Validity Username where
    isValid (Username "") = False
    isValid (Username t) = T.all validUsernameChar t

validUsernameChar :: Char -> Bool
validUsernameChar '-' = True
validUsernameChar '_' = True
validUsernameChar c =
    not (Char.isControl c) && Char.isAlphaNum c && Char.isLatin1 c

instance Hashable Username

instance PersistField Username where
    toPersistValue (Username t) = PersistText t
    fromPersistValue (PersistText t) =
        case parseUsername t of
            Nothing -> Left "Text isn't a valid username"
            Just un -> Right un
    fromPersistValue _ = Left "Not text"

instance PersistFieldSql Username where
    sqlType _ = SqlString

instance FromJSONKey Username where
    fromJSONKey = FromJSONKeyTextParser parseUsername

instance FromJSON Username where
    parseJSON = withText "Username" parseUsername

parseUsername :: MonadFail m => Text -> m Username
parseUsername t =
    case constructValid $ Username t of
        Nothing -> fail "Invalid username in JSON"
        Just un -> pure un

instance ToJSON Username where
    toJSON = toJSON . usernameText

instance ToJSONKey Username where
    toJSONKey = toJSONKeyText usernameText
