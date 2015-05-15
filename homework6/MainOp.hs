module MainOp where

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' f (x:xs)
    | f x       = x : filter f xs
    | otherwise = filter f xs
	
concat' :: [[a]] -> [a]
concat' [] = []
concat' (x:xs) = x ++ concat' xs

concatMap' :: (a ->[b]) -> [a] -> [b]
concatMap' _ [] = []
concatMap' f (x:xs) = f x ++ concatMap' f xs 

-------foldl  foldr------
filter_foldl ::(a -> Bool) -> [a] -> [a]
filter_foldl f array = foldl (\ acc el -> if (f el) then acc ++ [el] else acc) [] array

filter_foldr ::(a -> Bool) -> [a] -> [a]
filter_foldr f array = foldr (\ el acc -> if (f el) then el:acc else acc) [] array

concat_foldl :: [[a]] -> [a]
concat_foldl [] = []
concat_foldl array = foldl (\ acc el ->  acc ++ el ) [] array

concat_foldr :: [[a]] -> [a]
concat_foldr [] = []
concat_foldr array = foldr (\ el acc ->  el ++ acc) [] array

concatMap_foldl :: (a ->[b]) -> [a] -> [b]
concatMap_foldl f array = foldl (\ acc el -> acc ++ f el) [] array

concatMap_foldr :: (a ->[b]) -> [a] -> [b]
concatMap_foldr f array = foldr (\ el acc -> (f el) ++ acc) [] array



main = do 
		let ar = [[4,5,6],[4,5],[4,5,5]]
		let simple_array = [4,5,6]
		print (concat' ar)
		print (concat_foldl ar)
		print (concat_foldr ar)
		print (filter' (\ x -> x>5) simple_array)
		print (filter_foldl (\ x -> x>5) simple_array)
		print (filter_foldr (\ x -> x>5) simple_array)
		print (concatMap' (\x -> [(x,x+2,x/2)]) simple_array )
		print (concatMap_foldl (\x -> [(x,x+2,x/2)]) simple_array )
		print (concatMap_foldr (\x -> [(x,x+2,x/2)]) simple_array )