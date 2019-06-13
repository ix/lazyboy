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

{-# LANGUAGE MultiParamTypeClasses #-}

module Lazyboy.Control where

import           Control.Monad.Trans.RWS
import           Data.Word
import           Lazyboy.Types
import           Prelude                 hiding (not)

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
cond :: Condition -> Lazyboy a -> Lazyboy a
cond condition block = do
  label <- getLocalLabel
  tell [JPif condition (Name label)]
  a <- block
  tell [LABEL label]
  return a

-- | A typeclass for comparisons between registers and values.
class Comparable a b where
  equalTo     :: a -> b -> Lazyboy Condition -- ^ Check the equality of two items.
  notEqualTo  :: a -> b -> Lazyboy Condition -- ^ Check the inequality of two items.
  greaterThan :: a -> b -> Lazyboy Condition -- ^ Check whether `a` is greater than `b`.
  lessThan    :: a -> b -> Lazyboy Condition -- ^ Check whether `a` is less than `b`.

-- | An instance for comparing two 8-bit registers.
instance Comparable Register8 Register8 where
  equalTo A r  = tell [CPr r]            >> return NonZero
  equalTo r r' = tell [LDrr A r, CPr r'] >> return NonZero
  notEqualTo A r  = equalTo A r          >> return Zero
  notEqualTo r r' = equalTo r r'         >> return Zero
  greaterThan A r  = equalTo A r         >> return NoCarry
  greaterThan r r' = equalTo r r'        >> return NoCarry
  lessThan A r  = equalTo A r            >> return Carry
  lessThan r r' = equalTo r r'           >> return Carry

-- | An instance for comparing an 8-bit register and a Word8.
instance Comparable Register8 Word8 where
  equalTo A n = tell [CPn n]            >>  return NonZero
  equalTo r n = tell [LDrr A r, CPn n]  >>  return NonZero
  notEqualTo A n = equalTo A n          >>  return Zero
  notEqualTo r n = equalTo r n          >>  return Zero
  greaterThan A n = equalTo A n         >>  return NoCarry
  greaterThan r n = equalTo r n         >>  return NoCarry
  lessThan A n = equalTo A n            >>  return Carry
  lessThan r n = equalTo r n            >>  return Carry

-- | An instance for comparing a Word8 and an 8-bit register (this is an alias).
instance Comparable Word8 Register8 where
  equalTo = flip equalTo
  notEqualTo = flip notEqualTo
  greaterThan = flip greaterThan
  lessThan = flip lessThan

-- | Executes an action which returns a condition flag, then conditionally executes
-- another action baed on the state of that condition flag.
if' :: Lazyboy Condition -> Lazyboy a -> Lazyboy a
if' condition block = do
  flag <- condition
  cond flag block

-- | Boolean NOT operation for inverting Condition flags.
not :: Lazyboy Condition -> Lazyboy Condition
not action = do
  flag <- action
  return $ case flag of
    Zero    -> NonZero
    NonZero -> Zero
    Carry   -> NoCarry
    NoCarry -> Carry

-- | Assign boolean values to two registers based on the result flags of
-- some conditions and then AND them and return the result.
and :: Lazyboy Condition -> Lazyboy Condition -> Lazyboy Condition
and a b = do
  a' <- a
  cond a' $ do
    tell [LDrn L 1]
  b' <- b
  cond b' $ do
    tell [LDrn A 1]
  tell [ANDr L]
  return Zero

-- | Assign boolean values to two registers based on the result flags of
-- some conditions and then OR them and return the result.
or :: Lazyboy Condition -> Lazyboy Condition -> Lazyboy Condition
or a b = do
  a' <- a
  cond a' $ do
    tell [LDrn L 1]
  b' <- b
  cond b' $ do
    tell [LDrn A 1]
  tell [ORr L]
  return Zero

-- | An implementation of an imperative "while" loop.
while :: Lazyboy Condition -> Lazyboy () -> Lazyboy ()
while condition block = do
  loop <- getLocalLabel
  skip <- getLocalLabel
  tell [LABEL loop]
  if' (not condition) $ do
    tell [JP $ Name skip]
  block
  tell [JP $ Name loop]
  tell [LABEL skip]
