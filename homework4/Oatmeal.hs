module Test where

type OatmealTemp = Int

max_oatmeal_temp,min_oatmeal_temp,perfect_oatmeal_temp :: OatmealTemp
max_oatmeal_temp = 20
min_oatmeal_temp = 0
perfect_oatmeal_temp = 10

func_oatmeal :: OatmealTemp -> a
func_oatmeal temp
				| temp >= 0 || temp <= 20 = undefined

data Adjustment = NothingA | TurnLeft | TurnRight deriving(Show,Eq,Ord)

func_adjustment :: Adjustment -> a
func_adjustment adjustm = case adjustm of
								NothingA -> undefined
								TurnLeft -> undefined
								TurnRight -> undefined
								

								
to_adjustment :: OatmealTemp -> Adjustment								
to_adjustment x 
				| x < perfect_oatmeal_temp 	= TurnRight
				| x == perfect_oatmeal_temp = NothingA
				| x > perfect_oatmeal_temp 	= TurnLeft								
								
func_temperature :: OatmealTemp -> Adjustment
func_temperature t 
					|	t>0 || t<= 20 = to_adjustment t

