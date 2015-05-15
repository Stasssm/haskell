module RecursiveBinom where

bin :: Integer -> Integer -> Integer
bin n 0 = 1
bin n k = if (n==k) then 1
			else (bin (n-1) k) + (bin (n-1) (k-1)) 