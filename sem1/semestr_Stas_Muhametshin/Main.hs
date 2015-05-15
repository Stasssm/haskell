
import Terms

main = do
	putStrLn (print_tm [] term)
	putStrLn ("Answer: " ++ print_tm [] (evaluate term))
	
ex= do 
		putStrLn (print_tm [] tru)
		putStrLn ("Answer: " ++ print_tm [] (evaluate tru))
