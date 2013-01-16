{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  System.Console.Terminal.Bell
-- Copyright   :  (C) 2013 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
----------------------------------------------------------------------------
module System.Console.Terminal.Bell
  ( Bell(..)
  , _AudibleBellOnly
  , _AudibleBellPreferred
  , _VisibleBellOnly
  , _VisibleBellPreferred
  ) where

import Control.Lens
import Data.Data
import Data.Ix
import GHC.Generics

data Bell
  = AudibleBellOnly
  | AudibleBellPreferred
  | VisibleBellOnly
  | VisibleBellPreferred
  deriving (Eq,Ord,Show,Read,Ix,Enum,Bounded,Data,Typeable,Generic)

makePrisms ''Bell
