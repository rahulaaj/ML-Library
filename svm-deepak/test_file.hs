-- This is test file to demonstrate use of Linear 2-class SVM Library and its inbuilt kernel functions.

module Main where

	import Svm
	import Kernel_functions
	import Cga_code
	import Data.Array.Unboxed
	import System.IO
	import Text.ParserCombinators.Parsec
	import Data.CSV

	svm1 = LeastSquareSVM { kf=radialKernelFunction , param_list=[2.0] , cost = 1.0 }		-- to pass kernel-function as 'datatype KernelFunction' constructor 'KernelFunction' is called here.

	read_data_points:: Either a [[String]] -> Array_doublesList
	read_data_points list =
		let	read_aux [] count listL = listArray (1,count) listL
			read_aux (h:t) count listL =	read_aux t (count+1) (listL++[l'])			-- Count is no. of elements in array, listL is list of doubles_list
				where l' = map read h :: [Double]
		in
			case list of
				Right val -> read_aux val 0 []
				Left _ -> listArray (1,1) [[0.0]]			-- error case

	read_labels:: Either a [[String]] -> Array_doubles
	read_labels list =
		let	read_aux (h:t) count listL = read_aux t (count+1) (listL++[l'])
				where l' = read (head h) :: Double
			read_aux [] count listL = listArray (1,count) listL
		in
			case list of
				Right val -> read_aux val 0 []
				Left _ -> listArray (1,1) [0.0]			-- error case
				
	accuracy:: [Int] -> [Int] -> Double -> Double -> [Double]
	accuracy (x1:x1s) (x2:x2s) count total	| x1==x2 = accuracy x1s x2s count (total+1.0)
											| otherwise = accuracy x1s x2s (count+1.0) (total+1.0)
	accuracy _ _ count total = [count,total,((total-count)*100.0)/total]

	printSv (Solution_SVM a b c)= do 	print a

	main = do
			data_points_train <- parseFromFile csvFile "train_data.csv"
			labels_train <- parseFromFile csvFile "train_data_labels.csv"
			data_points_test <- parseFromFile csvFile "test_data.csv"
			labels_test <- parseFromFile csvFile "test_data_labels.csv"
			--print "data_points_test :"
			--print data_points_test
			--print "labels_test :"
			--print labels_test
			let dp_train = read_data_points data_points_train
			let dp_test = read_data_points data_points_test
			let lb_train = read_labels labels_train
			let lb_test = read_labels labels_test
			let sol = train_svm svm1 (DataSet dp_train lb_train) 0.5 (0)
			putStrLn "here1"
			print (weight_w sol)
			let lb_test_out  = test_svm svm1 sol dp_test
			let lb_test_out_rounded = map round lb_test_out
			putStrLn "test original :"
			let original_lb_test = map round (elems lb_test)
			print original_lb_test
			-- putStrLn "Sol :"
			-- printSv sol
			putStrLn "test output decimal :"
			print lb_test_out
			putStrLn "test output rounded :"
			print lb_test_out_rounded
			putStrLn "Accuracy :"
			print (accuracy original_lb_test lb_test_out_rounded 0.0 0.0)
