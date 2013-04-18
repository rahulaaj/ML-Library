import KMeans
import System.IO

main=do
	--print $ kmeans 2 [[1.0,1.0],[1.5,2.0],[3.0,4.0],[5.0,7.0],[3.5,5.0],[4.5,5.0],[3.5,4.5]]
	putStrLn "Sample Points"	
	putStrLn "[1.0,2.0,3.0]"
	putStrLn "[4.0,5.0,6.0]"
	putStrLn "[7.0,8.0,9.0]"
	putStrLn "[1.0,3.0,5.0]"
	putStrLn "[2.0,4.0,6.0]"
	putStrLn "[3.0,5.0,7.0]"
	putStrLn "[4.0,6.0,8.0]"
	putStrLn "[5.0,7.0,9.0]"
	putStrLn "[6.0,8.0,0.0]"
	putStrLn ""
	print $ kmeans 3 [[1.0,2.0,3.0],[4.0,5.0,6.0],[7.0,8.0,9.0],[1.0,3.0,5.0],[2.0,4.0,6.0],[3.0,5.0,7.0],[4.0,6.0,8.0],[5.0,7.0,9.0],[6.0,8.0,0.0]]
