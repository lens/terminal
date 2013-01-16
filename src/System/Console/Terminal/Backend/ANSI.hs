{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  System.Console.Terminal.Backend.ANSI
-- Copyright   :  (C) 2013 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
----------------------------------------------------------------------------
module System.Console.Terminal.Backend.ANSI
  ( Terminal(..)
  , runTerminal
  ) where

import Control.Applicative
import Control.Lens
import Control.Monad.State
import Data.Default
import qualified System.Console.ANSI as ANSI
import System.Console.Terminal.Class
import System.Console.Terminal.Color
import System.Console.Terminal.Color.Intense
import System.Console.Terminal.Intensity
import System.Console.Terminal.Style
import System.IO

#ifdef USE_UNIX
import System.Posix.IO (stdOutput)
import System.Posix.Terminal (queryTerminal)
#endif

runTerminal :: Terminal a -> IO a
runTerminal (Terminal m) = do
  (a,_) <- m def
  return a

newtype Terminal a = Terminal { unterminal :: Style -> IO (a, Style) }

instance Functor Terminal where
  fmap f (Terminal m) = Terminal $ \s -> do
    (a, t) <- m s
    return (f a, t)

instance Applicative Terminal where
  pure a = Terminal $ \s -> return (a, s)
  Terminal mf <*> Terminal ma = Terminal $ \s -> do
    (f, t) <- mf s
    (a, u) <- ma t
    return (f a, u)

instance Monad Terminal where
  return a = Terminal $ \s -> return (a, s)
  Terminal m >>= k = Terminal $ \s -> do
    (a, t) <- m s
    unterminal (k a) t

instance MonadIO Terminal where
  liftIO m = Terminal $ \s -> do
    a <- m
    return (a, s)

deltaStyle :: Style -> Style -> IO ()
deltaStyle s t = ANSI.setSGR $ join
    [ ANSI.Reset <$ guard hard
    , case t^.foreground of
       Just (IntenseColor i c) | t^.foreground /= r^.foreground -> [ ANSI.SetColor ANSI.Foreground (i^.from _Intensity) (c^.from _Color) ]
       _ -> []
    , case t^.background of
       Just (IntenseColor i c) | t^.background /= r^.background -> [ ANSI.SetColor ANSI.Background (i^.from _Intensity) (c^.from _Color) ]
       _ -> []
    , ANSI.SetUnderlining ANSI.SingleUnderline    <$ guard (t^.underlined && not (r^.underlined))
    , ANSI.SetConsoleIntensity ANSI.BoldIntensity <$ guard (t^.bold       && not (r^.bold))
    , ANSI.SetItalicized True                     <$ guard (t^.italic     && not (r^.italic))
    , ANSI.SetBlinkSpeed ANSI.SlowBlink           <$ guard (t^.blinking   && not (r^.blinking))
    ]
  where
    hard = s^.underlined && not (t^.underlined)
        || s^.bold && not (t^.bold)
        || s^.italic && not (t^.italic)
        || s^.blinking && not (t^.blinking)
        || has (foreground._Just) s && hasn't (foreground._Just) t
        || has (background._Just) s && hasn't (background._Just) t
    r | hard      = def
      | otherwise = s

instance MonadTerminal Terminal where
  setTitle = liftIO . ANSI.setTitle

  withStyle (StateT m) = Terminal $ \s -> do
    (a, t) <- m s
    deltaStyle s t
    return (a, t)

  emit = liftIO . putStr
  emitLn = liftIO . putStrLn

  ring _ = liftIO $ do
#ifdef USE_UNIX
    isTerm <- queryTerminal stdOutput
#else
    let isTerm = True
#endif
    when isTerm $ do
      putChar '\^G'
      hFlush stdout
