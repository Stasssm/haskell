module Terms where

import Data.Maybe

type BruijnIndex = Int

func_for_debruijin :: Int -> a
func_for_debruijin x = undefined x

type Argument = String

func_for_argument :: Argument -> a
func_for_argument x = undefined x

type Name = String 

func_for_name :: Name -> a
func_for_name x = undefined x

data Term = TmVar Name BruijnIndex |
			TmAbs Argument Term |
			TmApp Term Term

func_for_term :: Term -> a
func_for_term term = case term of
	 TmVar "" index -> undefined index
	 TmVar name _ -> undefined name
	 TmAbs name term1 -> undefined name term1
	 TmApp term1 term2 -> undefined term1 term2

	 
type Context = [String]

ctx :: Context
ctx = ["x", "y"]


func_for_context :: Context -> a
func_for_context [] = undefined
func_for_context (x:array) = func_for_context (array)

-- return tuple of new context and changed argument name 
pick_fresh_name :: Context -> Argument -> (Context, Argument)
pick_fresh_name ctx name
	| elem name ctx 	= pick_fresh_name ctx (name ++ "'")
	| otherwise 		= ((ctx ++ [name]), name)
	
-- printing term as described  in book 
print_tm :: Context -> Term -> String
print_tm ctx term = case term of
	TmAbs name absTerm -> 
		let (ctx', name') = pick_fresh_name ctx name
		in "(lambda " ++ name' ++ ". " ++ (print_tm ctx' absTerm) ++ ")"
	TmApp term1 term2 -> "(" ++ (print_tm ctx term1) ++ " " ++ (print_tm ctx term2) ++ ")"
	TmVar "" index -> 
		if (index < (length ctx)) 
			then ctx !! index
			else show index
	TmVar a _ -> a
	
-- needed for shifting
walk :: BruijnIndex -> Term -> Int -> Term
walk c term d = case term of
			TmVar "" index -> TmVar "" (index+d)
			TmVar a b -> TmVar a b
			TmAbs name term1 -> TmAbs name (walk (c+1) term1 d)
			TmApp term1 term2 -> TmApp (walk c term1 d) (walk c term2 d)

-- shifting			
term_shift :: Int -> Term -> Term
term_shift d term = walk 0 term d

term_subst :: BruijnIndex -> Term -> Term -> Term
term_subst index sub_term term = 
	walk 0 term
		where
			walk :: BruijnIndex -> Term -> Term
			walk c term = case term of
				TmVar "" varIndex ->
					if varIndex == index
						then term_shift c sub_term
					else TmVar "" varIndex
				TmVar a b -> TmVar a b
				TmAbs name term1 -> TmAbs name (walk (c+1) term1)
				TmApp term1 term2 -> TmApp (walk c term1) (walk c term2)
				--TmNamedVariable s -> TmNamedVariable s
				
term_subst_top :: Term -> Term -> Term
term_subst_top substitute_term term = term_shift (-1) (term_subst 0 (term_shift 1 substitute_term) term)

is_val :: Term -> Bool
is_val term = case term of
	TmAbs a b -> True
	_ -> False

evaluate_one :: Term -> Maybe Term
evaluate_one term = case term of
	TmApp t term2 -> case t of
		TmAbs _ termAbs -> Just (term_subst_top term2 termAbs)
		_ -> 
			if (is_val t) 
				then
					let term2' = evaluate_one term2
					in case term2' of
						Just t' -> Just(TmApp t t')
						Nothing -> Nothing
			else 
				let term1' = evaluate_one t
				in case term1' of
					Just t' -> Just (TmApp t' term2)
					Nothing -> Nothing
	_ -> Nothing


evaluate :: Term -> Term
evaluate term = 
	let term' = evaluate_one term
	in case term' of
		Just t -> evaluate t
		Nothing -> term

--simple terms described in book
tru :: Term
tru = (TmAbs "x" (TmAbs "y" (TmVar "" 0)))
-------
fls :: Term
fls = (TmAbs "x" (TmAbs "y" (TmVar "" 1)))
-------
if_expression :: Term
if_expression = (TmAbs "xIf" (TmAbs "yIf" (TmAbs "zIf" (TmApp (TmApp (TmVar "" 0) (TmVar "" 1)) (TmVar "" 2)))))

not' :: Term
not' = (TmAbs "n" (TmApp (TmApp (TmVar "" 0) (term_shift 1 fls)) (term_shift 1 tru)))
------
term :: Term
term = TmApp (TmApp (TmApp if_expression (TmApp not' tru)) (TmVar "then" 0)) (TmVar "else" 0)
	
--fls = (TmAbs "x" (TmAbs "y" (TmAbs "z"(TmVar "" 0)))