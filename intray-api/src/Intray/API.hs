{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Intray.API
    ( IntrayAPI
    , intrayAPI
    , IntraySite(..)
    , IntrayOpenAPI
    , intrayOpenAPI
    , IntrayOpenSite(..)
    , IntrayProtectedAPI
    , IntrayProtectedSite(..)
    , IntrayPublicAPI
    , IntrayPublicSite(..)
    , IntrayAdminAPI
    , IntrayAdminSite(..)
    , AuthCookie(..)
    , IntrayListItems
    , ListEntireIntray
    , ShowItem
    , IntraySize
    , AddItem
    , GetItem
    , DeleteItem
    , ItemType(..)
    , TypedItem(..)
    , textTypedItem
    , TypedItemCase(..)
    , typedItemCase
    , ItemInfo(..)
    , SyncRequest(..)
    , NewSyncItem(..)
    , SyncResponse(..)
    , Sync
    , AccountInfo(..)
    , GetAccountInfo
    , DeleteAccount
    , Registration(..)
    , Register
    , LoginForm(..)
    , Login
    , Docs
    , GetDocsResponse(..)
    , AdminStats(..)
    , GetAdminStats
    , AdminDeleteAccount
    , AdminGetAccounts
    , HashedPassword
    , passwordHash
    , validatePassword
    , ItemUUID
    , AccountUUID
    , Username
    , parseUsername
    , parseUsernameWithError
    , usernameText
    , module Data.UUID.Typed
    ) where

import Import

import Data.Aeson as JSON
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Char8 as SB8
import qualified Data.ByteString.Lazy as LB
import Data.List (nub)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time
import qualified Data.UUID as UUID
import Data.UUID.Typed

import Text.Blaze as HTML
import Text.Blaze.Html as HTML
import Text.Pandoc as Pandoc

import Web.Cookie

import Servant.API
import Servant.Auth
import Servant.Auth.Docs ()
import Servant.Auth.Server
import Servant.Auth.Server.SetCookieOrphan ()
import Servant.Docs
import Servant.Generic
import Servant.HTML.Blaze

import Intray.Data

intrayAPI :: Proxy IntrayAPI
intrayAPI = Proxy

type IntrayAPI = ToServant (IntraySite AsApi)

data IntraySite route = IntraySite
    { openSite :: route :- ToServant (IntrayOpenSite AsApi)
    , adminSite :: route :- "admin" :> ToServant (IntrayAdminSite AsApi)
    } deriving (Generic)

intrayOpenAPI :: Proxy IntrayOpenAPI
intrayOpenAPI = Proxy

type IntrayOpenAPI = ToServant (IntrayOpenSite AsApi)

data IntrayOpenSite route = IntrayOpenSite
    { protectedSite :: route :- ToServant (IntrayProtectedSite AsApi)
    , publicSite :: route :- ToServant (IntrayPublicSite AsApi)
    } deriving (Generic)

type IntrayProtectedAPI = ToServant (IntrayProtectedSite AsApi)

data IntrayProtectedSite route = IntrayProtectedSite
    { showItem :: route :- ShowItem
    , size :: route :- IntraySize
    , listItems :: route :- IntrayListItems
    , listEntireIntray :: route :- ListEntireIntray
    , addItem :: route :- AddItem
    , getItem :: route :- GetItem
    , deleteItem :: route :- DeleteItem
    , sync :: route :- Sync
    , accountInfo :: route :- GetAccountInfo
    , deleteAccount :: route :- DeleteAccount
    } deriving (Generic)

type IntrayPublicAPI = ToServant (IntrayPublicSite AsApi)

data IntrayPublicSite route = IntrayPublicSite
    { register :: route :- Register
    , login :: route :- Login
    , docs :: route :- Docs
    } deriving (Generic)

type ProtectAPI = Auth '[ JWT] AuthCookie

newtype AuthCookie = AuthCookie
    { authCookieUserUUID :: AccountUUID
    } deriving (Show, Eq, Generic, FromJSON, ToJSON)

instance FromJWT AuthCookie

instance ToJWT AuthCookie

type IntrayAdminAPI = ToServant (IntrayAdminSite AsApi)

data IntrayAdminSite route = IntrayAdminSite
    { adminStats :: route :- GetAdminStats
    , adminDeleteAccount :: route :- AdminDeleteAccount
    , adminGetAccounts :: route :- AdminGetAccounts
    } deriving (Generic)

-- | The item is not guaranteed to be the same one for every call if there are multiple items available.
type ShowItem
     = ProtectAPI :> "intray" :> "show-item" :> Get '[ JSON] (Maybe (ItemInfo TypedItem))

-- | Show the number of items in the intray
type IntraySize = ProtectAPI :> "intray" :> "size" :> Get '[ JSON] Int

-- | The order of the items is not guaranteed to be the same for every call.
type IntrayListItems
     = ProtectAPI :> "intray" :> "uuids" :> Get '[ JSON] [ItemUUID]

-- | The order of the items is not guaranteed to be the same for every call.
type ListEntireIntray
     = ProtectAPI :> "intray" :> "items" :> Get '[ JSON] [ItemInfo TypedItem]

type AddItem
     = ProtectAPI :> "intray" :> "item" :> ReqBody '[ JSON] TypedItem :> Post '[ JSON] ItemUUID

type GetItem
     = ProtectAPI :> "intray" :> "item" :> Capture "id" ItemUUID :> Get '[ JSON] (ItemInfo TypedItem)

instance ToCapture (Capture "id" ItemUUID) where
    toCapture _ = DocCapture "id" "The UUID of the item"

data TypedItem = TypedItem
    { itemType :: ItemType
    , itemData :: ByteString
    } deriving (Show, Read, Eq, Ord, Generic)

instance Validity TypedItem

instance FromJSON TypedItem where
    parseJSON =
        withObject "TypedItem" $ \o ->
            TypedItem <$> o .: "type" <*>
            (do t <- o .: "data"
                case Base64.decode $ SB8.pack t of
                    Left err ->
                        fail $
                        unwords
                            [ "Failed to decode base64-encoded typed item data:"
                            , err
                            ]
                    Right r -> pure r)

instance ToJSON TypedItem where
    toJSON TypedItem {..} =
        object
            ["type" .= itemType, "data" .= SB8.unpack (Base64.encode itemData)]

instance ToSample TypedItem where
    toSamples Proxy = singleSample $ TypedItem TextItem "Hello World!"

textTypedItem :: Text -> TypedItem
textTypedItem t = TypedItem {itemType = TextItem, itemData = TE.encodeUtf8 t}

typedItemCase :: TypedItem -> Either String TypedItemCase
typedItemCase TypedItem {..} =
    case itemType of
        TextItem -> left show $ CaseTextItem <$> TE.decodeUtf8' itemData

newtype TypedItemCase =
    CaseTextItem Text
    deriving (Show, Read, Eq, Ord, Generic)

data ItemInfo a = ItemInfo
    { itemInfoIdentifier :: ItemUUID
    , itemInfoContents :: a
    , itemInfoTimestamp :: UTCTime
    } deriving (Show, Read, Eq, Ord, Generic)

instance Validity a => Validity (ItemInfo a)

instance ToJSON a => ToJSON (ItemInfo a) where
    toJSON ItemInfo {..} =
        object
            [ "id" .= itemInfoIdentifier
            , "contents" .= itemInfoContents
            , "timestamp" .= itemInfoTimestamp
            ]

instance FromJSON a => FromJSON (ItemInfo a) where
    parseJSON =
        withObject "ItemInfo TypedItem" $ \o ->
            ItemInfo <$> o .: "id" <*> o .: "contents" <*> o .: "timestamp"

instance ToSample a => ToSample (ItemInfo a)

instance ToSample UTCTime where
    toSamples Proxy = singleSample $ UTCTime (fromGregorian 2018 2 10) 42

instance ToSample Text where
    toSamples Proxy = singleSample "Example Text"

instance ToSample (UUID a) where
    toSamples Proxy = singleSample (UUID $ UUID.fromWords 0 0 0 0)

instance ToSample Int where
    toSamples Proxy = singleSample 42

type DeleteItem
     = ProtectAPI :> "item" :> Capture "id" ItemUUID :> Delete '[ JSON] NoContent

type Sync
     = ProtectAPI :> "sync" :> ReqBody '[ JSON] SyncRequest :> Post '[ JSON] SyncResponse

data SyncRequest = SyncRequest
    { syncRequestUnsyncedItems :: [NewSyncItem]
    , syncRequestSyncedItems :: [ItemUUID]
    , syncRequestUndeletedItems :: [ItemUUID]
    } deriving (Show, Eq, Ord, Generic)

instance Validity SyncRequest where
    isValid = isValidByValidating
    validate SyncRequest {..} =
        mconcat
            [ syncRequestUnsyncedItems <?!> "syncRequestUnsyncedItems"
            , syncRequestSyncedItems <?!> "syncRequestSyncedItems"
            , syncRequestUndeletedItems <?!> "syncRequestUndeletedItems"
            , distinct syncRequestUnsyncedItems <?@>
              "Unsynced items are distinct"
            , distinct syncRequestSyncedItems <?@> "Synced items are distinct"
            , distinct syncRequestUndeletedItems <?@>
              "undeleted items are distinct"
            ]

instance FromJSON SyncRequest where
    parseJSON =
        withObject "SyncRequest" $ \o ->
            SyncRequest <$> o .: "unsynced" <*> o .: "synced" <*>
            o .: "undeleted"

instance ToJSON SyncRequest where
    toJSON SyncRequest {..} =
        object
            [ "unsynced" .= syncRequestUnsyncedItems
            , "synced" .= syncRequestSyncedItems
            , "undeleted" .= syncRequestUndeletedItems
            ]

instance ToSample SyncRequest

data NewSyncItem = NewSyncItem
    { newSyncItemContents :: TypedItem
    , newSyncItemTimestamp :: Maybe UTCTime
    } deriving (Show, Eq, Ord, Generic)

instance Validity NewSyncItem where
    isValid = isValidByValidating
    validate NewSyncItem {..} =
        mconcat
            [ newSyncItemContents <?!> "newSyncItemContents"
            , newSyncItemTimestamp <?!> "newSyncItemTimestamp"
            ]

instance FromJSON NewSyncItem where
    parseJSON v =
        (NewSyncItem <$> parseJSON v <*> pure Nothing) <|>
        withObject
            "NewSyncItem"
            (\o -> NewSyncItem <$> o .: "contents" <*> o .:? "timestamp")
            v

instance ToJSON NewSyncItem where
    toJSON NewSyncItem {..} =
        case newSyncItemTimestamp of
            Nothing -> toJSON newSyncItemContents
            Just ts ->
                object ["contents" .= newSyncItemContents, "timestamp" .= ts]

instance ToSample NewSyncItem

data SyncResponse = SyncResponse
    { syncResponseAddedItems :: [ItemInfo TypedItem]
    , syncResponseNewRemoteItems :: [ItemInfo TypedItem]
    , syncResponseItemsToBeDeletedLocally :: [ItemUUID]
    } deriving (Show, Eq, Ord, Generic)

instance Validity SyncResponse where
    isValid = isValidByValidating
    validate SyncResponse {..} =
        mconcat
            [ syncResponseAddedItems <?!> "syncResponseAddedItems"
            , syncResponseNewRemoteItems <?!> "syncResponseNewRemoteItems"
            , syncResponseItemsToBeDeletedLocally <?!>
              "syncResponseItemsToBeDeletedLocally"
            , distinct syncResponseAddedItems <?@> "Added items are distinct"
            , distinct syncResponseNewRemoteItems <?@> "new items are distinct"
            , distinct syncResponseItemsToBeDeletedLocally <?@>
              "deleted items are distinct"
            ]

instance FromJSON SyncResponse where
    parseJSON =
        withObject "SyncResponse" $ \o ->
            SyncResponse <$> o .: "added" <*> o .: "new" <*> o .: "deleted"

instance ToJSON SyncResponse where
    toJSON SyncResponse {..} =
        object
            [ "added" .= syncResponseAddedItems
            , "new" .= syncResponseNewRemoteItems
            , "deleted" .= syncResponseItemsToBeDeletedLocally
            ]

instance ToSample SyncResponse

type GetAccountInfo = ProtectAPI :> "account" :> Get '[ JSON] AccountInfo

data AccountInfo = AccountInfo
    { accountInfoUUID :: AccountUUID
    , accountInfoUsername :: Username
    , accountInfoCreatedTimestamp :: UTCTime
    , accountInfoLastLogin :: Maybe UTCTime
    , accountInfoAdmin :: Bool
    } deriving (Show, Eq, Ord, Generic)

instance Validity AccountInfo

instance FromJSON AccountInfo where
    parseJSON =
        withObject "AccountInfo" $ \o ->
            AccountInfo <$> o .: "uuid" <*> o .: "username" <*> o .: "created" <*>
            o .: "last-login" <*>
            o .: "admin"

instance ToJSON AccountInfo where
    toJSON AccountInfo {..} =
        object
            [ "uuid" .= accountInfoUUID
            , "username" .= accountInfoUsername
            , "created" .= accountInfoCreatedTimestamp
            , "last-login" .= accountInfoLastLogin
            , "admin" .= accountInfoAdmin
            ]

instance ToSample AccountInfo

type DeleteAccount = ProtectAPI :> "account" :> Delete '[ JSON] NoContent

data Registration = Registration
    { registrationUsername :: Username
    , registrationPassword :: Text
    } deriving (Show, Eq, Ord, Generic)

instance Validity Registration

instance ToJSON Registration where
    toJSON Registration {..} =
        object
            ["name" .= registrationUsername, "password" .= registrationPassword]

instance FromJSON Registration where
    parseJSON =
        withObject "Registration Text" $ \o ->
            Registration <$> o .: "name" <*> o .: "password"

instance ToSample Registration

type Register
     = "item" :> ReqBody '[ JSON] Registration :> Post '[ JSON] NoContent

data LoginForm = LoginForm
    { loginFormUsername :: Username
    , loginFormPassword :: Text
    } deriving (Show, Eq, Ord, Generic)

instance Validity LoginForm

instance FromJSON LoginForm where
    parseJSON =
        withObject "LoginForm" $ \o ->
            LoginForm <$> o .: "username" <*> o .: "password"

instance ToJSON LoginForm where
    toJSON LoginForm {..} =
        object
            ["username" .= loginFormUsername, "password" .= loginFormPassword]

instance ToSample LoginForm

instance ToSample Username

instance ToSample SetCookie where
    toSamples Proxy = singleSample def

type Login
     = "login" :> ReqBody '[ JSON] LoginForm :> PostNoContent '[ JSON] (Headers '[ Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] NoContent)

type Docs = Get '[ HTML] GetDocsResponse

newtype GetDocsResponse = GetDocsResponse
    { unGetDocsResponse :: HTML.Html
    } deriving (Generic)

instance MimeUnrender HTML GetDocsResponse where
    mimeUnrender Proxy bs = do
        pandoc <-
            left show $
            Pandoc.readHtml def $ T.unpack $ TE.decodeUtf8 $ LB.toStrict bs
        pure $ GetDocsResponse $ Pandoc.writeHtml def pandoc

instance ToSample GetDocsResponse where
    toSamples Proxy = singleSample $ GetDocsResponse "Documentation (In HTML)."

instance ToMarkup GetDocsResponse where
    toMarkup (GetDocsResponse html) = toMarkup html

distinct :: Eq a => [a] -> Bool
distinct ls = length ls == length (nub ls)

type GetAdminStats = ProtectAPI :> "stats" :> Get '[ JSON] AdminStats

data AdminStats = AdminStats
    { adminStatsNbUsers :: Int
    , adminStatsNbItems :: Int
    } deriving (Show, Eq, Ord, Generic)

instance Validity AdminStats

instance FromJSON AdminStats where
    parseJSON =
        withObject "AdminStats" $ \o ->
            AdminStats <$> o .: "users" <*> o .: "items"

instance ToJSON AdminStats where
    toJSON AdminStats {..} =
        object ["users" .= adminStatsNbUsers, "items" .= adminStatsNbItems]

instance ToSample AdminStats

type AdminDeleteAccount
     = ProtectAPI :> "account" :> Capture "id" AccountUUID :> Delete '[ JSON] NoContent

type AdminGetAccounts = ProtectAPI :> "accounts" :> Get '[ JSON] [AccountInfo]
