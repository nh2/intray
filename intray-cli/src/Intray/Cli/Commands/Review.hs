{-# LANGUAGE OverloadedStrings #-}

module Intray.Cli.Commands.Review
    ( review
    ) where

import Import

import Intray.Cli.Commands.Done
import Intray.Cli.Commands.Show
import Intray.Cli.OptParse
import Intray.Cli.Prompt

review :: CliM ()
review = do
    showItem
    res <- liftIO $ prompt "done [y/N]"
    case res of
        "y" -> doneItem
        "Y" -> doneItem
        _ -> pure ()
    review
