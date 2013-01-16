{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  System.Console.Terminal.Style
-- Copyright   :  (C) 2013 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
----------------------------------------------------------------------------
module System.Console.Terminal.Style
  ( Style(..)
  , HasStyle(..)
  ) where

import Control.Lens
import System.Console.Terminal.Color.Intense

data Style = Style
  { _foreground :: Maybe IntenseColor
  , _background :: Maybe IntenseColor
  , _underlined :: Bool
  , _blinking   :: Bool
  , _bold       :: Bool
  , _italic     :: Bool
  }

makeClassy ''Style
