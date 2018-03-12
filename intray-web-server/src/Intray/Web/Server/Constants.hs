{-# LANGUAGE CPP #-}

module Intray.Web.Server.Constants where

import Import

development :: Bool
#ifdef DEVELOPMENT
development = True
#else
development = False
#endif
