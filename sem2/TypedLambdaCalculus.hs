module TypedLambdaCalculus where

data Type = TyBool | TyArr Type Type | TySimple String deriving(Show,Eq)

func_type :: Type -> a
func_type type' = case type' of
				TyBool -> undefined 
				TyArr t1 t2 -> undefined t1 t2
				TySimple name -> undefined name

data Term = TmVar Int Type | TmAbs String Type Term | TmApp Term Term
						|  TmTrue | TmFalse | TmIf Term Term Term  deriving(Show,Eq)

func_term :: Term -> a
func_term term = case term of
				TmVar deb type' -> undefined deb type'
				TmAbs name type' term -> undefined name type' term
				TmApp term1 term2 -> undefined term1 term2
				TmTrue -> undefined
				TmFalse -> undefined
				TmIf term1 term2 term3 -> undefined term1 term2 term3
				
get_type :: Term -> Type
get_type t = case t of
				TmTrue -> TyBool
				TmFalse -> TyBool
				TmIf t1 t2 t3  ->
					if (get_type t1) == TyBool then
						let tyT2 = get_type t2 in
						if tyT2 == (get_type t3) then tyT2
						else error "arms of conditional have different types"
					else error "guard of conditional not a boolean"
				TmVar deb type' -> type'
				TmAbs name type' term -> TyArr type' (get_type term)
				TmApp term1 term2 -> 
							let tyT1 = get_type term1 in
							let tyT2 = get_type term2 in
							case tyT1 of
									TyArr tyT11 tyT12 ->
											if tyT2 == tyT11
												then tyT12
												else error "parameter type mismatch"
									_-> error "arrow type expected"
									
main = do
	let term =TmIf TmTrue (TmApp (TmAbs "t" (TySimple "tIn") (TmAbs "s" (TySimple "simS") (TmVar (0) (TySimple "lIn"))) ) (TmVar (0) (TySimple "tIn")))
							(TmApp (TmAbs "t" (TySimple "tIn") (TmAbs "s" (TySimple "simS") (TmVar (0) (TySimple "lIn"))) ) (TmVar (0) (TySimple "tIn")))
	let term2 = TmApp (TmAbs "t" (TySimple "tIn") (TmAbs "s" (TySimple "simS") (TmVar (0) (TySimple "lIn"))) ) (TmVar (0) (TySimple "tIn"))
	print (get_type term2)