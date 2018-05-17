{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Intray.API.Admin.Gen where

import Import

import Data.GenValidity
import Data.GenValidity.ByteString ()
import Data.GenValidity.Text ()
import Data.GenValidity.Time ()
import Data.GenValidity.UUID ()

import Intray.API
import Intray.Data.Gen ()

instance GenUnchecked AdminStats

instance GenValid AdminStats where
    genValid = (AdminStats <$> genValid <*> genValid) `suchThat` isValid
