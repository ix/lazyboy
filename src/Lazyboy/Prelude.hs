{-|
    Module      : Lazyboy.Prelude
    Description : Convenience aliases for Lazyboy which share names with Haskell's Prelude.
    Copyright   : (c) Rose 2019
    License     : BSD3
    Maintainer  : rose@lain.org.uk
    Stability   : experimental
    Portability : POSIX

    This module defines aliases for Lazyboy functions which share names with entities in Haskell's
    Prelude library. These are presented for the user to optionally import.
-}

module Lazyboy.Prelude where

import Lazyboy (Condition, Lazyboy, Comparable)
import qualified Lazyboy.Control as Lazyboy

-- | Overload the == (equality) operator for use in Lazyboy.
(==) :: Comparable a b => a -> b -> Lazyboy Condition
(==) = Lazyboy.equalTo

-- | Overload the /= (inequality) operator for use in Lazyboy.
(/=) :: Comparable a b => a -> b -> Lazyboy Condition
(/=) = Lazyboy.notEqualTo

-- | Overload the > (greater than) operator for use in Lazyboy.
(>) :: Comparable a b => a -> b -> Lazyboy Condition
(>) = Lazyboy.greaterThan

-- | Overload the < (lesser than) operator for use in Lazyboy.
(<) :: Comparable a b => a -> b -> Lazyboy Condition
(<) = Lazyboy.lessThan

-- | Overload the && (AND) operator for use in Lazyboy.
(&&) :: Lazyboy Condition -> Lazyboy Condition -> Lazyboy Condition
(&&) = Lazyboy.and

-- | Overload the || (OR) operator for use in Lazyboy.
(||) :: Lazyboy Condition -> Lazyboy Condition -> Lazyboy Condition
(||) = Lazyboy.or
