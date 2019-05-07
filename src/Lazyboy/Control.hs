{-|
    Module      : Lazyboy.Control
    Description : Control flow features for Lazyboy
    Copyright   : (c) Rose 2019
    License     : BSD3
    Maintainer  : rose@lain.org.uk
    Stability   : experimental
    Portability : POSIX

    This module defines methods of controlling the flow of execution for Lazyboy.
-}

module Lazyboy.Control where

import           Control.Monad.Trans.RWS
import           Data.Word
import           Lazyboy.Types

-- | Execute an action within a global label and pass the action the label.
withLabel :: (Label -> Lazyboy ()) -> Lazyboy ()
withLabel block = do
  label <- Global <$> get
  modify (+ 1) -- increment the label name counter
  tell [LABEL label]
  block label

-- | Execute an action within a local label and pass the action the label.
withLocalLabel :: (Label -> Lazyboy ()) -> Lazyboy ()
withLocalLabel block = do
  label <- Local <$> get
  modify (+ 1) -- increment the label name counter
  tell [LABEL label]
  block label

-- | Embed a file and return a (global) label for it.
embedFile :: FilePath -> Lazyboy Label
embedFile file = do
    label <- Global <$> get
    tell [LABEL label, INCLUDE file]
    return label

-- | Embed an image and return a (global) label for it.
embedImage = embedFile

-- | Embed a sequence of bytes into the file and return a (global) label for it.
embedBytes :: [Word8] -> Lazyboy Label
embedBytes bytes = do
  label <- Global <$> get
  tell [LABEL label, BYTES bytes]
  return label

-- | Suspend execution indefinitely by jumping infinitely.
freeze :: Lazyboy ()
freeze = loop $ return ()
  where loop block = do
          label <- Local <$> get
          modify (+ 1)
          tell [LABEL label]
          block
          tell [JUMP label]

-- | Executes the given action provided condition flag is set.
cond :: Condition -> Lazyboy () -> Lazyboy ()
cond condition block = do
  label <- Local <$> get
  modify (+ 1)
  tell [JUMPif condition label]
  block
  tell [LABEL label]
