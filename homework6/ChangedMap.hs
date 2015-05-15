module ChangedMap where

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map f xs

map_foldl :: (a -> b) -> [a] -> [b]
map_foldl _ [] = []
map_foldl f array = foldl (\ acc el ->  acc ++[f el] ) [] array

map_foldr :: (a -> b) -> [a] -> [b]
map_foldr _ [] = []
map_foldr f array = foldr (\ el acc -> (f el) : acc ) [] array


simple_func :: Int -> Int
simple_func a = a+5 

main = do 
		let ar = [5,4,9,12,98]
		print(map simple_func ar)
		print(map' simple_func ar)
		print(map_foldl simple_func ar)
		print(map_foldr simple_func ar)