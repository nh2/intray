{-# LANGUAGE RecordWildCards #-}

module Intray.Server.Item
    ( makeIntrayItem
    , makeItemInfo
    ) where

import Data.Time

import Intray.API
import Intray.Data

makeIntrayItem :: AccountUUID -> ItemUUID -> UTCTime -> TypedItem -> IntrayItem
makeIntrayItem u i ts TypedItem {..} =
    IntrayItem
        { intrayItemIdentifier = i
        , intrayItemType = itemType
        , intrayItemContents = itemData
        , intrayItemTimestamp = ts
        , intrayItemUserId = u
        }

makeItemInfo :: IntrayItem -> ItemInfo TypedItem
makeItemInfo IntrayItem {..} =
    ItemInfo
        { itemInfoIdentifier = intrayItemIdentifier
        , itemInfoContents =
              TypedItem
                  {itemType = intrayItemType, itemData = intrayItemContents}
        , itemInfoTimestamp = intrayItemTimestamp
        }
