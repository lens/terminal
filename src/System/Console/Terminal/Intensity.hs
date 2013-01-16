{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  System.Console.Terminal.Intensity
-- Copyright   :  (C) 2013 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
----------------------------------------------------------------------------
module System.Console.Terminal.Intensity
  ( Intensity(..)
  , AsIntensity(..)
  , HasIntensity(..)
  ) where

import Control.Applicative
import Control.Lens
import Data.Ix
import System.Console.Terminal.Util

#ifdef USE_ANSI
import qualified System.Console.ANSI as ANSI
#endif

data Intensity
  = Dull
  | Vivid
  deriving (Eq,Ord,Show,Read,Ix,Enum,Bounded)

makeClassy ''Intensity

class AsIntensity p f t where
  -- |
  -- @
  -- '_Intensity' :: 'Equality'' 'Intensity' 'Intensity'
  -- '_Intensity' :: 'Iso''      'ANSI.ColorIntensity' 'Intensity'
  -- @
  _Intensity :: Overloaded' p f t Intensity

  -- |
  -- @
  -- '_Dull'  :: 'Prism'' 'Intensity' ()
  -- '_Vivid' :: 'Prism'' 'Intensity' ()
  -- ...
  -- @
  _Dull, _Vivid :: (Choice p, Applicative f) => Overloaded' p f t ()

  _Dull    = _Intensity.eq Dull
  _Vivid   = _Intensity.eq Vivid

instance AsIntensity p f Intensity where
  _Intensity = id

#ifdef USE_ANSI
instance (Profunctor p, Functor f) => AsIntensity p f ANSI.ColorIntensity where
  _Intensity = iso sa bt where
    bt Dull  = ANSI.Dull
    bt Vivid = ANSI.Vivid
    sa ANSI.Dull  = Dull
    sa ANSI.Vivid = Vivid
  _Dull  = en ANSI.Dull
  _Vivid = en ANSI.Vivid

instance HasIntensity ANSI.ColorIntensity where
  intensity = _Intensity

#endif
