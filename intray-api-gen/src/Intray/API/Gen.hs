{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-dodgy-exports #-}

module Intray.API.Gen
    ( module Intray.API.Gen
    , module Intray.API.Account.Gen
    , module Intray.API.Admin.Gen
    , module Intray.API.Protected.Gen
    ) where

import Import

import Data.GenValidity
import Data.GenValidity.ByteString ()
import Data.GenValidity.Text ()
import Data.GenValidity.Time ()
import Data.GenValidity.UUID ()

import Intray.API
import Intray.Data.Gen ()

import Intray.API.Account.Gen ()
import Intray.API.Admin.Gen ()
import Intray.API.Protected.Gen ()


instance GenUnchecked Registration

instance GenValid Registration where
    genValid = (Registration <$> genValid <*> genValid) `suchThat` isValid

instance GenUnchecked LoginForm

instance GenValid LoginForm where
    genValid = (LoginForm <$> genValid <*> genValid) `suchThat` isValid
