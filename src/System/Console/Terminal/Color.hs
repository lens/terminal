{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
  , HasColor(..)
  ) where

import Control.Applicative
import Control.Lens
import Data.Ix
import Data.Data
import GHC.Generics
import System.Console.Terminal.Util
import qualified System.Console.ANSI as ANSI

#ifdef USE_TERMINFO
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
  deriving (Eq,Ord,Show,Read,Ix,Enum,Bounded,Data,Typeable,Generic)

makeClassy ''Color

class AsColor p f t where
  -- |
  -- @
  -- '_Color' :: 'Equality'' 'Color' 'Color'
  -- '_Color' :: 'Iso''      'ANSI.Color' 'Color'
  -- '_Color' :: 'Prism''    'Terminfo.Color' 'Color'
  -- @
  _Color :: Optic' p f t Color

  -- |
  -- @
  -- '_Black' :: 'Prism'' 'Color' ()
  -- '_Red'   :: 'Prism'' 'Color' ()
  -- ...
  -- @
  _Black, _Red, _Green, _Yellow, _Blue, _Magenta, _Cyan, _White :: (Choice p, Applicative f) => Optic' p f t ()

  _Black   = _Color.eq Black
  _Red     = _Color.eq Red
  _Green   = _Color.eq Green
  _Yellow  = _Color.eq Yellow
  _Blue    = _Color.eq Blue
  _Magenta = _Color.eq Magenta
  _Cyan    = _Color.eq Cyan
  _White   = _Color.eq White

instance AsColor p f Color where
  _Color = id

#ifdef USE_TERMINFO
instance (Choice p, Applicative f) => AsColor p f Terminfo.Color where
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
  _Black   = eq Terminfo.Black
  _Red     = eq Terminfo.Red
  _Green   = eq Terminfo.Green
  _Yellow  = eq Terminfo.Yellow
  _Blue    = eq Terminfo.Blue
  _Magenta = eq Terminfo.Magenta
  _Cyan    = eq Terminfo.Cyan
  _White   = eq Terminfo.White
#endif

instance (Profunctor p, Functor f) => AsColor p f ANSI.Color where
  _Color = iso sa bt where
    bt Black   = ANSI.Black
    bt Red     = ANSI.Red
    bt Green   = ANSI.Green
    bt Yellow  = ANSI.Yellow
    bt Blue    = ANSI.Blue
    bt Magenta = ANSI.Magenta
    bt Cyan    = ANSI.Cyan
    bt White   = ANSI.White
    sa ANSI.Black   = Black
    sa ANSI.Red     = Red
    sa ANSI.Green   = Green
    sa ANSI.Yellow  = Yellow
    sa ANSI.Blue    = Blue
    sa ANSI.Magenta = Magenta
    sa ANSI.Cyan    = Cyan
    sa ANSI.White   = White
  _Black   = en ANSI.Black
  _Red     = en ANSI.Red
  _Green   = en ANSI.Green
  _Yellow  = en ANSI.Yellow
  _Blue    = en ANSI.Blue
  _Magenta = en ANSI.Magenta
  _Cyan    = en ANSI.Cyan
  _White   = en ANSI.White

instance HasColor ANSI.Color where
  color = _Color
