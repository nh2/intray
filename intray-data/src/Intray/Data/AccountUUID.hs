module Intray.Data.AccountUUID
    ( AccountUUID
    ) where

import Data.UUID.Typed

import Intray.Data.UUID ()

type AccountUUID = UUID User

data User
