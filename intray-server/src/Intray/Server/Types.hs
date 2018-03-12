module Intray.Server.Types where

import Control.Monad.Reader

import Database.Persist.Sqlite

import Servant
import Servant.Auth.Server

import Intray.API

data IntrayServerEnv = IntrayServerEnv
    { envConnectionPool :: ConnectionPool
    , envCookieSettings :: CookieSettings
    , envJWTSettings :: JWTSettings
    , envAdmins :: [Username]
    }

type IntrayHandler = ReaderT IntrayServerEnv Handler
