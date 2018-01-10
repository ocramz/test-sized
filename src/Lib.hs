{-# language KindSignatures, DataKinds, RankNTypes, ScopedTypeVariables, TypeApplications #-}
{-# language TypeFamilies #-}
-- {-# language GADTs #-}
-- {-# language MultiParamTypeClasses #-}
module Lib where

import GHC.TypeLits (Nat, natVal, KnownNat)
import Data.Proxy

import Control.Applicative

import Data.Semigroup
import Control.Category





newtype V (n :: Nat) a = MkV [a] deriving (Eq, Show)

mkV :: forall n a . KnownNat n => [a] -> Maybe (V n a)
mkV l | len == lenl = Just (MkV l)
      | otherwise = Nothing
  where
    lenl = length l
    len = fromIntegral $ natVal (Proxy @n)


instance forall n a . (Num a, KnownNat n) => Monoid (V n a) where
  mappend ((MkV v1s) :: V n a) ((MkV v2s) :: V n a) = MkV $ zipWith (+) v1s v2s
  mempty = MkV $ replicate (fromIntegral $ natVal (Proxy @n)) 0 -- V (natVal (Proxy @n)) $ replicate n 0

lift1 :: (a -> b) -> V n a -> V n b
lift1 f (MkV x :: V n a) = MkV $ f <$> x

lift2 :: (a -> a -> b) -> V n a -> V n a -> V n b
lift2 f (MkV x :: V n a) (MkV y :: V n a) = MkV $ zipWith f x y



-- | 'Dim' type from hmatrix Internal.Static : https://hackage.haskell.org/package/hmatrix-0.18.1.0/docs/src/Internal-Static.html


-- newtype Dim (n :: Nat) t = Dim t deriving (Show)

-- lift1F :: (a -> b) -> Dim n a -> Dim n b
-- lift1F f (Dim v) = Dim (f v)


-- newtype Dim2 (m :: Nat) (n :: Nat) t = Dim2 t


-- | open type families

type family Dim1 (t :: Nat -> * -> *) :: *

-- type instance Dim1 V = [a]

-- type instance Dim1 (n :: Nat) 
  

-- type instance Dim1 (n :: Nat) (V (n :: Nat) a) = [a]
  

-- type family Dim2 (m :: Nat) (n :: Nat) t :: *







-- | A typeclass 

-- class Dim1 n v where
--   dim1 :: proxy n -> Int





-- v0 = mkV [1,2,3] :: Maybe (V 2 Int)
