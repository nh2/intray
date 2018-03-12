module Intray.Data.UserUUID
    ( UserUUID
    ) where

import Data.UUID.Typed

import Intray.Data.UUID ()

type UserUUID = UUID User

data User
