import NaiveBayes
import System.IO

user cs f =
	case f of
		"start" -> do
						putStrLn "Enter an action [train/classify/exit] :"
						action <- getLine
						user cs action
		"train" -> do
						utStrLn "Category :"
						category <- getLine
						putStrLn "Text :"
						text <- getLine
						user ( train cs category text ) "start"
		"classify" -> do
						putStrLn "Text :"
						text <- getLine
						let res = classify cs text
						putStrLn $ fst res
						putStr "Probability :"
						putStrLn . show $ snd res
						putStrLn "\n"
						user cs "start"
		"exit" -> do
			putStrLn "\n"

main = do
	user (Classifier []) "start"
							
