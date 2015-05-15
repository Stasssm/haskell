module TailBinom where

bin :: Integer -> Integer -> Integer
tailbin _  _  0 _ = 0
tailbin ac1 ac2 _ 0 = ac1 `div` ac2
tailbin ac1 ac2 a b = tailbin (ac1 * a) (ac2 * b) (a-1) (b-1)
bin = tailbin 1 1
