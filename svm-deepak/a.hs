-- This file contains SVM class and its instance implementation for least squares support vector machine. SVM class uses CGA for getting optimal solution to langrangian.

{-# OPTIONS_GHC -XBangPatterns #-}			-- For bang in function arguements to avoid laziness

module Svm where		-- module name and file_name starts with caps, it is made to get imported to test_file.hs

	import Kernel_functions
	import Cga_code
	import Data.Array.Unboxed
	   
	data Solution_SVM = Solution_SVM {weight_w::Array_doubles, sv::Array_doublesList, bias::Double}	-- weight_w is array of dual weights and sv are support vectors

	data LeastSquareSVM = LeastSquareSVM {kf::[Double] -> [Double] -> [Double] -> Double, param_list::[Double], cost::Double}
	-- param_list is input parameter list to kernel function
	-- cost is cost-coefficient of Lagrangian equation.

	derivative_cost	:: LeastSquareSVM -> Double
	evaluateKernel	:: LeastSquareSVM -> [Double] -> [Double] -> Double
	train_svm	:: LeastSquareSVM -> DataSet -> Double -> Int -> Solution_SVM
	test_svm	:: LeastSquareSVM -> Solution_SVM -> Array_doublesList -> [Double]

	-- evaluateKernel : evaluate the user defined kernel_function on two data points. To be defined in instance of SVM class.
	-- derivative_cost : derivative of cost function in svm. It is added to diagonal elements of kernel matrix. It helps in preventing the overfitting of the training data. To be defined in instance of class.

	--This function creates KernelMatrix. 
	--Input is sa::SVM LeastSquareSVM and x:array of data_points (all the data points, each element of array is list of doubles)
	--output is KernelMatrix K[i,j] = kernelFunction x[i] x[j], stored as lower left triangluar symmetric matrix
	createKernelMatrix	:: LeastSquareSVM -> Array_doublesList -> KernelMatrix
	createKernelMatrix sa x =
		let	n = snd $ bounds x
			dim = quot ((n+1) * n) 2
			evaluate i j = if (i == j) then evaluateKernel sa (x!i) (x!j) + derivative_cost sa
							else evaluateKernel sa (x!i) (x!j)
			km = listArray (1, dim) [evaluate i j | j <- indices x, i <- range(1,j)]
		in 
			KernelMatrix km

	-- For training- input: training dataset with labels, epsilon and max_iter for conjugate gradient algorithm
	-- For testing- output: Support vector solution i.e. weights,support vector,bias b.
	train_svm sa (DataSet data_points labels) epsilon max_iter =
		let	n = snd $ bounds labels
			x_initial = listArray (1, n) $ replicate n 0
			list_ones = listArray (1, n) $ replicate n 1
			km = createKernelMatrix sa data_points
			nu = cga x_initial list_ones list_ones km epsilon max_iter		--Solving for AX=I case
			v = cga x_initial labels labels km epsilon max_iter				--Solving for AX=Y case
			bias = (aSum v) / (aSum nu)
			weight_w = aZipWith (\ x y -> x - bias*y) v nu
		in
			Solution_SVM weight_w data_points bias

	--Takes input solution and test_datapoints, gives the list of labels as output.
	--Labels y = f(x), where f is svm represented generating function
	test_svm sa (Solution_SVM weight_w sv bias) data_points =
			let	val x = listArray (bounds sv) [evaluateKernel sa x v | v <- elems sv]
				evaluate x = aDot weight_w $ val x
			in
				[(evaluate p) + bias | p <- elems data_points]

	-- =============================================================================================================

	instance SVM LeastSquareSVM where
	  derivative_cost = (0.5 /) . cost			-- derivative_cost when supplied with livsvm applies cost on it i.e. derivative_cost l = .. . cost l , here cost l is Double.. cost :: LSSVM LeastSquareSVM -> Double
	  evaluateKernel (LeastSquareSVM kf param_list cost) xs ys = kf xs ys param_list		-- here param_list is local variable::[Double] and not ::LSSVM -> [Double] .. local scoping
	  
	-- =============================================================================================================

