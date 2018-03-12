{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Intray.Client.Gen where

import Import

import Intray.Client.Store

import Intray.Data.Gen ()

instance GenUnchecked StoreItem

instance GenValid StoreItem

instance GenUnchecked Store

instance GenValid Store
