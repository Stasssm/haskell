module BinarySearchTree where

data Tree = EmptyTree | Node Int Tree Tree deriving (Show,Read, Eq) 

singleton :: Int -> Tree  
singleton x = Node x EmptyTree EmptyTree  
  
treeInsert :: Int -> Tree -> Tree  
treeInsert x EmptyTree = singleton x  
treeInsert x (Node a left right)   
    | x == a = Node x left right  
    | x < a  = Node a (treeInsert x left) right  
    | x > a  = Node a left (treeInsert x right)  
	
treeHeight :: Tree -> Int
treeHeight tree = case tree of 
	EmptyTree -> 0
	Node _ tree1 tree2 -> (max (treeHeight tree1) (treeHeight tree2)) +1
	
sumNode :: Tree -> Int
sumNode tree = case tree of 
	EmptyTree -> 0
	Node s left right -> s + sumNode (left) + sumNode(right)
	
treeElem :: Int -> Tree -> String  
treeElem x EmptyTree = "There is no such element in the tree" 
treeElem x (Node a left right)  
    | x == a = "This element in the tree"
    | x < a  = treeElem x left  
    | x > a  = treeElem x right  
	
search' :: Int -> Tree -> Tree
search' elToFound tree = case tree of
									EmptyTree -> EmptyTree
									Node el leftEl rightEl  ->
										if elToFound == el then tree
										else if elToFound < el then leftEl
										else rightEl
								
search_element :: Int -> Tree -> Tree 
search_element x EmptyTree = EmptyTree
search_element x (Node a left right)  
    | x == a = Node a left right
    | x < a  = search_element x left  
    | x > a  = search_element x right  
	
main = do 
		let nums = [8,9,4,5,1,3,0]  
		let numsTree = foldr treeInsert EmptyTree nums
		print (numsTree)
		print(sumNode numsTree)
		print(treeHeight numsTree)
		print(treeElem 5 numsTree)
		print(search_element 5 numsTree)
	