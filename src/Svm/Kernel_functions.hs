-- This file contains implementation of some commonly used kernel functions. Most recommended one is radial basis kernel.

{-# OPTIONS_GHC -XBangPatterns #-}			-- For bang in function arguements to avoid laziness

module Svm.Kernel_functions where


	
	import Data.List (foldl')

	{- Kernel function gives inner product in feature space. Input- fisrt vector, second vector, parameter list -}
	
	norm (x:xs) (y:ys) = foldl' (+) 0 $ zipWith (\x y -> (x-y)**2) xs ys

	--1. This is the kernel function with radial basis. p is the value sigma^2 and ps can be empty
	radialKernelFunction :: [Double] -> [Double] -> [Double] -> Double
	radialKernelFunction xvector yvector (p:ps) = exp $ (norm xvector yvector)/(-p)

	--2. This linear kernel function. Simply a dot product.
	linearKernelFunction :: [Double] -> [Double] -> [Double] -> Double
	linearKernelFunction xs ys p = foldl' (+) 0 $ zipWith (*) xs ys
		
	--3. This is polynomial kernel function.
	polyKernelFunction :: [Double] -> [Double] -> [Double] -> Double
	polyKernelFunction xvector yvector (p0:p1:ps) = (p0 + (foldl' (+) 0 $ zipWith (*) xvector yvector))**p1
	
	--4. This is quadratic kernel function.
	quadraticKernelFunction :: [Double] -> [Double] -> [Double] -> Double
	quadraticKernelFunction xvector yvector (p0:ps) = (p0 + linearKernelFunction xvector yvector ps)**2
	quadraticKernelFunction xvector yvector [] = (1 + linearKernelFunction xvector yvector [])**2
	
	--5. This is cubic kernel function.
	cubicKernelFunction :: [Double] -> [Double] -> [Double] -> Double
	cubicKernelFunction xvector yvector (p0:ps) = (p0 + linearKernelFunction xvector yvector ps)**3
	cubicKernelFunction xvector yvector [] = (1 + linearKernelFunction xvector yvector [])**3

	--6. This is histogram kernel function.
	histKernelFunction :: [Double] -> [Double] -> [Double] -> Double
	histKernelFunction (x:xs) (y:ys) ps	| x < y = x + histKernelFunction xs ys ps
										| otherwise = y +  histKernelFunction xs ys ps
	histKernelFunction _ _ _ = 0
	
	--7. This is sigmoid kernel function.	
	sigmoidKernelFunction :: [Double] -> [Double] -> [Double] -> Double
	sigmoidKernelFunction xvector yvector (p0:p1:ps) = tanh (p0 * (foldl' (+) 0 $ zipWith (*) xvector yvector) - p1)
	
	-- =============================================================================================================
