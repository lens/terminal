{-# LANGUAGE CPP #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  System.Console.Terminal.Util
-- Copyright   :  (C) 2013 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- This internal module provides some combinators that help write prisms.
----------------------------------------------------------------------------
module System.Console.Terminal.Util
  ( en
  ) where

import Data.Function
import Control.Lens

en :: Enum a => a -> Prism' a ()
en a = nearly a (((==) `on` fromEnum) a)
