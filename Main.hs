
module Main where

import Geometric

main = do
  print (geo' [1..100] :: [Int])
  print (geo' [1,3..100] :: [Int])
  print (take 5 $ geo [1,3/2..] :: [Rational])
  print (take 5 $ geo [3,1..] :: [Rational])
  print (take 5 $ geo [2,1..] :: [Double])

