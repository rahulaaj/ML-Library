--This file contains implementation of Conjugate Gradient Algorithm which is used to find optimal solution of Langrangian. It finds
--the saddle point of the dual of the Lagrangian.

module Svm.Cga_code where

	import Data.Array.Unboxed
	import Data.List (foldl')
	import Numeric.LinearAlgebra
	-- algebra for matrices
	-- Unboxed arrays are used for eager evaluation of elements of array. (performance improve)
	-- foldl' gives better performance than sum due to eager evaluation and tailoptimisation.

	type Array_doubles = UArray Int Double			-- Array of doubles
	type Array_doublesList = Array Int [Double]		-- Array of list of doubles
	data DataSet = DataSet {data_points::Array_doublesList, labels::Array_doubles}	-- Dataset contains a list of vectors(data_points) and labels for training i.e. f(x) = y forall {x,y}
	newtype KernelMatrix = KernelMatrix (Matrix Double)	

	-- Conjugate Gradient Algorithm: It will run until a cutoff delta or max_iter is reached.
	-- x:initially assumed solution , p:initial p_0 , r:initial r_0 , km: matrix A , epsilon:cutoff delta , max_iter:maximum number of iterations
	-- Acknowledgement: Refer wikipedia for iterative version of conjugate algorithm.
	conjugateGradientAlgo :: KernelMatrix -> Array_doubles -> Array_doubles -> Array_doubles -> Double -> Int -> Array_doubles
	conjugateGradientAlgo (KernelMatrix km) x p r epsilon max_iter = cga_aux delta_initial max_iter x p r False
		 where 	
				delta_initial = aDot r r
				cga_aux _ _ x _ _ True = x			--break loop
				cga_aux delta iter x p r _ =
					let	km_p = km <> (buildMatrix (rows km) 1 (\(i,j) -> (p!(i+1))))		-- n*1 matrix
						oneByOne = ((buildMatrix 1 (rows km) (\(i,j) -> (p!(j+1)))) <> km_p)
						alpha1 = delta / (det oneByOne)
						x_new = aAdd x $ scalar_mult alpha1 (elems p)
						r_new = aAdd r $ scalar_mult (-alpha1) (toList $ flatten km_p)
						delta_new = aDot r_new r_new
						p_new = aAdd r_new $ scalar_mult (delta_new/delta) (elems p)
						done = (delta_new < epsilon*delta_initial) || (iter == 0)
					in  
						cga_aux delta_new (iter-1) x_new p_new r_new done


	--Following helper functions over arrays have been used :

	--Multiply each element with a scalar
	scalar_mult :: Double -> [Double] -> Array_doubles
	scalar_mult a l = 
			let	temp = map (*a) l
				len = length temp
			in
				(listArray (1,len) temp)

	--ZipWith for arrays
	aZipWith :: (Double -> Double -> Double) -> Array_doubles -> Array_doubles -> Array_doubles
	aZipWith f a1 a2 = array (bounds a1) [(i,f (a1!i) (a2!i)) | i <- indices a1]

	--Sum of all elements
	aSum :: Array_doubles -> Double
	aSum ar = foldl' (+) 0 (elems ar)

	--Dot product for arrays
	aDot :: Array_doubles -> Array_doubles -> Double
	aDot a1 a2 = aSum $ aZipWith (*) a1 a2

	--Adding two arrays elementwise
	aAdd :: Array_doubles -> Array_doubles -> Array_doubles
	aAdd a1 a2 = aZipWith (+) a1 a2
	-- =============================================================================================================
