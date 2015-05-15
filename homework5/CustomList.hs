module CustomList where

data List a b = Empty | ElementOne a (List a b) | ElementTwo b (List a b) deriving (Eq,Show,Read)

emptyList :: List a b
emptyList = Empty

createExample :: List Int [Char]
createExample = ElementOne 5 (ElementTwo "4" (ElementOne 5 (ElementTwo "7" Empty)))

length' :: List a b -> Int
length' list = case list of 
				Empty -> 0
				ElementOne _ tail -> 1+ length' tail
				ElementTwo _ tail -> 1+ length' tail

dmap :: List a b -> (a->c) ->(b -> v) -> List c v
dmap list func1 func2 = case list of 
							Empty -> Empty
							ElementOne el tail -> ElementOne (func1 el) (dmap tail func1 func2)
							ElementTwo el tail  -> ElementTwo (func2 el) (dmap tail func1 func2)
							
function ::  Int -> Int
function a = a+5 