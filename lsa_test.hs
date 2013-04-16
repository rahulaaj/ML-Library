import lsa
documents = ["Romeo and Juliet","Juliet happy dagger","Romeo died by dagger","Live free or die, that's the New Hampshire's motto","Did you know , New Hapshire is in England"]

main = do
	let 
		let (a,b,c)=lsa documents 2
		print c
