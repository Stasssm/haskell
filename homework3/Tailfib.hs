module Tailfib where

fib :: Integer -> Integer
tailfib 0 a b = a
tailfib n a b = tailfib (n-1) b (a+b)
fib n = tailfib n 0 1