{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  System.Console.Terminal.Color.Intense
-- Copyright   :  (C) 2013 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
----------------------------------------------------------------------------
module System.Console.Terminal.Color.Intense
  ( IntenseColor(IntenseColor)
  ) where

import Control.Lens
import System.Console.Terminal.Color
import System.Console.Terminal.Intensity
import Data.Data
import GHC.Generics

data IntenseColor = IntenseColor { _intenseColorIntensity :: Intensity, _intenseColorColor :: Color }
  deriving (Eq,Ord,Show,Read,Data,Typeable,Generic)

makeLenses ''IntenseColor

instance HasColor IntenseColor where
  color = intenseColorColor

instance HasIntensity IntenseColor where
  intensity = intenseColorIntensity
