{-# LANGUAGE NoImplicitPrelude, UnicodeSyntax, DeriveDataTypeable #-}

module Data.Number.TF
    ( TF(N, NegInf, PosInf, NaN)
    ) where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

-- from base:
import Data.Bool     ( Bool(False, True) )
import Data.Data     ( Data )
import Data.Eq       ( Eq )
import Data.Maybe    ( Maybe(Nothing, Just) )
import Data.Ord      ( Ordering(LT, GT) )
import Data.Typeable ( Typeable )
import Prelude       ( Num, (+), (*), (-), negate, abs, signum, fromInteger )
import Text.Read     ( Read )
import Text.Show     ( Show )

-- from base-unicode-symbols:
import Data.Function.Unicode ( (∘) )

-- from logfloat:
import Data.Number.PartialOrd  ( PartialOrd, cmp )
import Data.Number.Transfinite ( Transfinite
                               , infinity, negativeInfinity, isInfinite
                               , notANumber, isNaN
                               )


--------------------------------------------------------------------------------
-- Transfinite numbers
--------------------------------------------------------------------------------

-- | Extends an existing type with the concept of infinity.
data TF α = N α | NegInf | PosInf | NaN
            deriving (Eq, Read, Show, Data, Typeable)

instance (Num α) ⇒ Num (TF α) where
    N x    + N y    = N (x + y)
    N _    + NegInf = NegInf
    N _    + PosInf = PosInf
    NegInf + N _    = NegInf
    NegInf + NegInf = NaN
    NegInf + PosInf = NaN
    PosInf + N _    = PosInf
    PosInf + NegInf = NaN
    PosInf + PosInf = PosInf
    NaN    + _      = NaN
    _      + NaN    = NaN

    N x    * N y    = N (x * y)
    N 0    * NegInf = NaN
    N _    * NegInf = NegInf
    N 0    * PosInf = NaN
    N _    * PosInf = PosInf
    NegInf * N 0    = NaN
    NegInf * N _    = NegInf
    NegInf * NegInf = NaN
    NegInf * PosInf = NaN
    PosInf * N 0    = NaN
    PosInf * N _    = PosInf
    PosInf * NegInf = NaN
    PosInf * PosInf = PosInf
    NaN    * _      = NaN
    _      * NaN    = NaN

    N x    - N y    = N (x - y)
    N _    - NegInf = NegInf
    N _    - PosInf = PosInf
    NegInf - N _    = NegInf
    NegInf - NegInf = NaN
    NegInf - PosInf = NaN
    PosInf - N _    = PosInf
    PosInf - NegInf = NaN
    PosInf - PosInf = NaN
    NaN    - _      = NaN
    _      - NaN    = NaN

    negate (N x)  = N (negate x)
    negate NegInf = PosInf
    negate PosInf = NegInf
    negate NaN    = NaN

    abs (N x)  = N (abs x)
    abs NegInf = PosInf
    abs PosInf = PosInf
    abs NaN    = NaN

    signum (N x)  = N (signum x)
    signum NegInf = N (negate 1)
    signum PosInf = N 1
    signum NaN    = N (negate 1)

    fromInteger = N ∘ fromInteger

instance (PartialOrd α) ⇒ PartialOrd (TF α) where
    cmp NegInf NegInf = Nothing
    cmp NegInf PosInf = Just LT
    cmp NegInf (N _)  = Just LT

    cmp PosInf NegInf = Just GT
    cmp PosInf PosInf = Nothing
    cmp PosInf (N _)  = Just GT

    cmp (N _)  NegInf = Just GT
    cmp (N _)  PosInf = Just LT
    cmp (N x)  (N y)  = cmp x y

    cmp NaN    _     = Nothing
    cmp _      NaN   = Nothing

instance (PartialOrd α) ⇒ Transfinite (TF α) where
    infinity          = PosInf

    negativeInfinity  = NegInf

    notANumber        = NaN

    isInfinite PosInf = True
    isInfinite NegInf = True
    isInfinite _      = False

    isNaN      NaN    = True
    isNaN      _      = False
