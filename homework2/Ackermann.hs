module Ackermann where

ak :: Int->Int->Int
ak 0 n = n+1
ak m 0 = ak (m-1) 1
ak m n = ak (m-1) (ak m (n-1))