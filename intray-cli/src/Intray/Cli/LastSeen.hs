module Intray.Cli.LastSeen
    ( LastItem(..)
    , writeLastSeen
    , readLastSeen
    , clearLastSeen
    ) where

import Import

import Intray.Client.Store

import Intray.Cli.JSON
import Intray.Cli.OptParse
import Intray.Cli.Path

readLastSeen :: CliM (Maybe LastItem)
readLastSeen = do
    p <- lastSeenItemPath
    readJSON p

writeLastSeen :: LastItem -> CliM ()
writeLastSeen i = do
    p <- lastSeenItemPath
    writeJSON p i

clearLastSeen :: CliM ()
clearLastSeen = do
    p <- lastSeenItemPath
    liftIO $ ignoringAbsence $ removeFile p
