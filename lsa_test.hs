import LSA
import Numeric.LinearAlgebra

documents = ["Romeo and Juliet","Juliet happy dagger","Romeo died by dagger","Live free or die, that is the New-Hampshire motto","Did you know , New-Hampshire is in England"]
main = do
	let (a,b,c)=lsa (documents) 2
	print c
