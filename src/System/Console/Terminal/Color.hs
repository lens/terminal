{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  System.Console.Terminal.Color
-- Copyright   :  (C) 2013 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
----------------------------------------------------------------------------
module System.Console.Terminal.Color
  ( Color(..)
  , AsColor(..)
  ) where

import Control.Lens
import Data.Ix
#ifdef TERMINFO
import qualified System.Console.Terminfo.Color as Terminfo
#endif

data Color
  = Black
  | Red
  | Green
  | Yellow
  | Blue
  | Magenta
  | Cyan
  | White
  deriving (Eq,Ord,Show,Read,Ix,Enum,Bounded)

eq :: Eq a => a -> Prism' a ()
eq a = prism' (const a) $ \b -> if a == b then Just () else Nothing

class AsColor t where
  _Color :: Prism' t Color

  _Black, _Red, _Green, _Yellow, _Blue, _Magenta, _Cyan, _White :: Prism' t ()

  _Black   = _Color.eq Black
  _Red     = _Color.eq Red
  _Green   = _Color.eq Green
  _Yellow  = _Color.eq Yellow
  _Blue    = _Color.eq Blue
  _Magenta = _Color.eq Magenta
  _Cyan    = _Color.eq Cyan
  _White   = _Color.eq White

instance AsColor Color where
  _Color = id

#ifdef TERMINFO
instance AsColor Terminfo.Color where
  _Color = prism' bt seta where
    bt Black   = Terminfo.Black
    bt Red     = Terminfo.Red
    bt Green   = Terminfo.Green
    bt Yellow  = Terminfo.Yellow
    bt Blue    = Terminfo.Blue
    bt Magenta = Terminfo.Magenta
    bt Cyan    = Terminfo.Cyan
    bt White   = Terminfo.White
    seta Terminfo.Black         = Just Black
    seta Terminfo.Red           = Just Red
    seta Terminfo.Green         = Just Green
    seta Terminfo.Yellow        = Just Yellow
    seta Terminfo.Blue          = Just Blue
    seta Terminfo.Magenta       = Just Magenta
    seta Terminfo.Cyan          = Just Cyan
    seta Terminfo.White         = Just White
    seta Terminfo.ColorNumber{} = Nothing
#endif
