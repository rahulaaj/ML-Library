import LSA
import Numeric.LinearAlgebra

documents = ["Romeo and Juliet","Juliet happy dagger","Romeo died by dagger","Live free or die, that is the New-Hampshire motto","Did you know , New-Hampshire is in England"]
main = do
	putStrLn "Sample doc"
	let (a,b,c)=lsa (documents) 2
	putStrLn "Romeo and Juliet"
	putStrLn "Juliet happy dagger"
	putStrLn "Romeo died by dagger"
	putStrLn "Live free or die, that is the New-Hampshire motto"
	putStrLn "Did you know , New-Hampshire is in England"
	putStrLn ""
	print c
