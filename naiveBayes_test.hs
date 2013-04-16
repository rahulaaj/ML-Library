import NaiveBayes
import System.IO

user cs f =
	case f of
		"start" -> do
						putStrLn "Enter an action [train/classify] :"
						action <- getLine
						user cs action
		"train" -> do
						putStr "Category :"
						category <- getLine
						putStr "Text :"
						text <- getLine
						user ( train cs category text ) "start"
		"classify" -> do
						putStr "Text :"
						text <- getLine
						putStr $ fst $ classify cs text 
						putStr ":"
						putStr $ snd $ classify cs text
						putStr "\n\n\n"
						user cs "start"
		_ -> do
			user cs "start"

main = do
	user cs "start"
							
