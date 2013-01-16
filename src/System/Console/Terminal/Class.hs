-----------------------------------------------------------------------------
-- |
-- Module      :  System.Console.Terminal.Class
-- Copyright   :  (C) 2013 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
----------------------------------------------------------------------------
module System.Console.Terminal.Class
  ( MonadTerminal(..)
  ) where

import Control.Monad.State
import System.Console.Terminal.Bell
import System.Console.Terminal.Style

class MonadIO m => MonadTerminal m where
  setTitle :: String -> m ()
  withStyle :: StateT Style IO a -> m a
  emit :: String -> m ()
  ring :: Bell -> m ()
