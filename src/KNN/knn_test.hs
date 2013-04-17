import KNN

main = do
	putStrLn "Sample Points"
	putStrLn "a [1,2,3]"
	putStrLn "b [7,8,9]"
	putStrLn "a [2,1,1]"
	putStrLn "a [0,1,1]"
	putStrLn "b [7,8,8]"
	putStrLn "b [8,7,9]"
	putStrLn "a [2,3,1]"
	putStrLn ""
	putStrLn "To classify [7,5,9]"
	print $ knn [("a",[1,2,3]),("b",[7,8,9]),("a",[2,1,1]),("a",[0,1,1]),("b",[7,8,8]),("b",[8,7,9]),("a",[2,3,1])] [7,5,9] 3

