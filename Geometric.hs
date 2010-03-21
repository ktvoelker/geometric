
module Geometric (geo, geo', (%)) where

import qualified Data.Ratio as R

data Geo = Geo Rational deriving (Eq, Ord, Show)

instance Num Geo where
  (+)    (Geo a) (Geo b)    = Geo (a + b)
  (*)    (Geo a) (Geo b)    = Geo (a * b)
  (-)    (Geo a) (Geo b)    = Geo (a - b)
  abs    (Geo a)            = Geo (abs a)
  signum (Geo a)            = Geo (signum a)
  fromInteger               = Geo . toRational

instance Real Geo where
  toRational (Geo a)        = toRational a

instance Fractional Geo where
  (/)    (Geo a) (Geo b)    = Geo (a / b)
  fromRational              = Geo

instance RealFrac Geo where
  properFraction (Geo a)    = case properFraction a of (i, f) -> (i, Geo f)

(%) :: (Integral a) => a -> a -> Geo
a % b = Geo ((toInteger a) R.% (toInteger b))

tmap2 :: (a -> b) -> (a, a) -> (b, b)
tmap2 f (x, y) = (f x, f y)

geo :: (Fractional n) => [Geo] -> [n]
geo = map (fromRational . toRational)

geo' :: (Integral n) => [Geo] -> [n]
geo' = map truncate . geo

def_factor = 2

instance Enum Geo where
  succ           (Geo n)  = Geo $ succ n
  pred           (Geo n)  = Geo $ pred n
  toEnum                  = Geo . toEnum
  fromEnum       (Geo n)  = fromEnum n
  enumFrom       n        = enumFromThen n (n * def_factor)
  enumFromThen   n n'     = let q = n' / n in iterate (* q) n
  enumFromTo     n end    = enumFromThenTo n (n * def_factor) end
  enumFromThenTo n n' end
    | n  > end             = []
    | n' > end             = [n]
    | otherwise            = takeWhile (<= end) $ enumFromThen n n'

