-- This file contains SVM class and its instance implementation for least squares support vector machine. SVM class uses CGA for getting optimal solution to langrangian.

{-# OPTIONS_GHC -XBangPatterns #-}			-- For bang in function arguements to avoid laziness

module Svm where		-- module name and file_name starts with caps, it is made to get imported to test_file.hs

	import Kernel_functions
	import Cga_code
	import Data.Array.Unboxed
	import Numeric.LinearAlgebra
	   
	data Solution_SVM = Solution_SVM {sv::Array_doublesList, weight_w::Array_doubles, bias::Double}	-- weight_w is array of dual weights and sv are support vectors

	data LeastSquareSVM = LeastSquareSVM {kf::[Double] -> [Double] -> [Double] -> Double, param_list::[Double], cost::Double}
	-- param_list is input parameter list to kernel function
	-- cost is cost-coefficient of Lagrangian equation.

	--This function creates KernelMatrix. 
	--Input is sa::LeastSquareSVM and x:array of data_points (all the data points, each element of array is list of doubles)
	--output is KernelMatrix K[i,j] = kernelFunction x[i] x[j]
	buildKMatrix :: Array_doublesList -> LeastSquareSVM -> KernelMatrix
	buildKMatrix x sa = 
				let	n = snd $ bounds x
					evaluate (i,j) = if (i == j) then evaluateKernel (x!(i+1)) (x!(j+1)) sa + derivative_cost sa
										else evaluateKernel (x!(i+1)) (x!(j+1)) sa
				in KernelMatrix (buildMatrix n n evaluate)

	-- For training- input: training dataset with labels, epsilon and max_iter for conjugate gradient algorithm
	-- For testing- output: Support vector solution i.e. weights,support vector,bias b.
	train_svm	:: DataSet -> Double -> Int -> LeastSquareSVM -> Solution_SVM
	train_svm (DataSet data_points labels) epsilon max_iter sa=
		let	n = snd $ bounds labels
			x_initial = listArray (1, n) $ replicate n 0
			list_ones = listArray (1, n) $ replicate n 1
			km = buildKMatrix data_points sa
			nu = conjugateGradientAlgo km x_initial list_ones list_ones epsilon max_iter		--Solving for AX=I case
			v = conjugateGradientAlgo km x_initial labels labels epsilon max_iter				--Solving for AX=Y case
			bias = (aSum v) / (aSum nu)
			weight_w = aZipWith (\ x y -> x - bias*y) v nu
		in
			Solution_SVM data_points weight_w bias

	--Takes input solution and test_datapoints, gives the list of labels as output.
	--Labels y = f(x), where f is svm represented generating function
	test_svm	:: Solution_SVM -> Array_doublesList -> LeastSquareSVM -> [Double]
	test_svm (Solution_SVM sv weight_w bias) data_points sa=
			let	val x = listArray (bounds sv) [evaluateKernel x v sa | v <- elems sv]
				evaluate x = aDot weight_w $ val x
			in
				[(evaluate p) + bias | p <- elems data_points]

	-- derivative_cost : derivative of cost function in svm. It is added to diagonal elements of kernel matrix. It helps in preventing the overfitting of the training data. To be defined in instance of class.
	derivative_cost	:: LeastSquareSVM -> Double
	derivative_cost sa = (1 /) $ (2*(cost sa))			-- derivative_cost when supplied with livsvm applies cost on it i.e. derivative_cost l = .. . cost l , here cost l is Double.. cost :: LSSVM LeastSquareSVM -> Double

	-- evaluateKernel : evaluate the user defined kernel_function on two data points. To be defined in instance of SVM class.
	evaluateKernel	:: [Double] -> [Double] -> LeastSquareSVM -> Double
	evaluateKernel xs ys (LeastSquareSVM kf param_list cost) = kf xs ys param_list		-- here param_list is local variable::[Double] and not ::LSSVM -> [Double] .. local scoping
	  
	-- =============================================================================================================

