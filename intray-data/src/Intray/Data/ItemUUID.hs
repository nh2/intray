module Intray.Data.ItemUUID
    ( ItemUUID
    ) where

import Data.UUID.Typed

import Intray.Data.UUID ()

type ItemUUID = UUID Item

data Item
