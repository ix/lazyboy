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

-- | Get a label, and in the process increment the counter used to track labels.
-- this provides a safe interface to label retrieval and utilization.
getLabel :: Lazyboy Integer
getLabel = do 
  label <- get
  modify (+ 1)
  return label

-- | Get a local label. The name is guaranteed to be unique.
getLocalLabel :: Lazyboy Label
getLocalLabel = Local <$> getLabel

-- | Get a global label. The name is guaranteed to be unique.
getGlobalLabel :: Lazyboy Label
getGlobalLabel = Global <$> getLabel

-- | Execute an action within a global label and pass the action the label.
withLabel :: (Label -> Lazyboy ()) -> Lazyboy ()
withLabel block = do
  label <- getGlobalLabel
  tell [LABEL label]
  block label

-- | Execute an action within a local label and pass the action the label.
withLocalLabel :: (Label -> Lazyboy ()) -> Lazyboy ()
withLocalLabel block = do
  label <- getLocalLabel 
  tell [LABEL label]
  block label

-- | Embed a file and return a global label for it.
-- A jump over the block of data is added to prevent the image data being executed.
embedFile :: FilePath -> Lazyboy Label
embedFile file = do
    label <- getGlobalLabel 
    skipLabel <- getGlobalLabel
    tell [JP $ Name skipLabel]
    tell [LABEL label, INCLUDE file]
    tell [LABEL skipLabel]
    return label

-- | Embed an image and return a (global) label for it.
-- A jump over the block of data is added to prevent the image data being executed.
embedImage = embedFile

-- | Embed a sequence of bytes into the file and return a (global) label for it.
-- A jump over the block of data is added to prevent the image data being executed.
embedBytes :: [Word8] -> Lazyboy Label
embedBytes bytes = do
  label <- getGlobalLabel
  skipLabel <- getGlobalLabel
  tell [JP $ Name skipLabel]
  tell [LABEL label, BYTES bytes]
  tell [LABEL skipLabel]
  return label

-- | Suspend execution indefinitely by disabling interrupts and halting.
freeze :: Lazyboy ()
freeze = withLabel $ \label -> do
  tell [DI, HALT]
  tell [JP $ Name label]

-- | Executes the given action provided condition flag is set.
cond :: Condition -> Lazyboy () -> Lazyboy ()
cond condition block = do
  label <- getLocalLabel 
  tell [JPif condition (Name label)]
  block
  tell [LABEL label]
