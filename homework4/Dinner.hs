module Dinner where

data DinnerOrder = NothingD | Chicken | Pasta deriving(Show,Eq,Ord)

func_order :: DinnerOrder -> a
func_order order = case order of
			 NothingD -> undefined
			 Chicken -> undefined
			 Pasta -> undefined
								
dinner_order_to_msg :: DinnerOrder -> String
dinner_order_to_msg dinner = case dinner of
								 NothingD -> "The passenger don't order anything"
								 Chicken -> "The passenger ordered chicken."
								 Pasta -> "The passenger ordered pasta."
