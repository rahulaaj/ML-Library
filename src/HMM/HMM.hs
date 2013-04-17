--we implement forward, backward, viterbi, baum-welch algorithms in this code

--HMM Class :
--- has states, observations, start_probability (states X 1), transition_probability (states X states), emission_probability (states X observations)

import Debug.Trace
import Data.Maybe
import Data.Array
import Data.List
import Data.List.Extras
import Data.Number.LogFloat
import qualified Data.MemoCombinators as Memo
-- import Control.Parallel
import System.IO

type ProbType = LogFloat

data HMM stateType observationType = HMM {states :: [stateType]
										, observations :: [observationType]
										, startProbability :: stateType ->  ProbType
										, transitionProbability :: stateType -> stateType -> ProbType
										, emissionProbability :: stateType -> observationType -> ProbType
									}

instance (Show stateType, Show observationType) => Show (HMM stateType observationType) where
    show = hmmToString 
    
hmmToString hmm = "HMM" ++ "{ states =" ++ (show $ states hmm)
                     ++ "\n\n observations =" ++ (show $ observations hmm)
                     ++ "\n\n startProbabilities =" ++ (show [(s,startProbability hmm s) | s <- states hmm])
                     ++ "\n\n transitionProbabilities =" ++ (show [(s1,s2,transitionProbability hmm s1 s2) | s1 <- states hmm, s2 <- states hmm])
                     ++ "\n\n emissionProbabilities =" ++ (show [(s,o,emissionProbability hmm s o) | s <- states hmm, o <- observations hmm])
                     ++ "}"

-----------Helper Functions-------------------
----------------------------------------------

stateIndex :: (Show stateType, Eq stateType, Show observationType) => HMM stateType observationType -> stateType -> Int
stateIndex hmm s = fromMaybe
    (seq (error ("error in stateIndex : " ++ show s ++ "not is possible states")) 0)
    (elemIndex s $ states hmm)

observationIndex :: (Show stateType, Show observationType, Eq observationType) => HMM stateType observationType -> observationType -> Int
observationIndex hmm o = fromMaybe
    (seq (error ("error in observationIndex : " ++ show o ++ "not is possible observations")) 0)
    (elemIndex o $ observations hmm)

elemIndex2 :: (Show a, Eq a) => a -> [a] -> Int
elemIndex2 e list = fromMaybe
	(seq (error ("elemIndex2: Index "++show e++" not in HMM "++show list)) 0) 
	(elemIndex e list)
-----------------------------------------------


-------------------------------------------------------------
---Code for simpleHMM from HMM Library-----------------------
---reference : HMM Library-----------------------------------
--http://hackage.haskell.org/packages/archive/hmm/0.2.1.1/doc/html/src/Data-HMM.html-----

-- | Use simpleHMM to create an untrained hidden Markov model
simpleHMM :: (Eq stateType, Show eventType, Show stateType) => 
             [stateType] -> [eventType] -> HMM stateType eventType
simpleHMM sL eL = HMM { states = sL
                      , observations = eL
                      , startProbability = const evenDist--skewedDist s
                      , transitionProbability = \ s1 s2 -> skewedDist s2
                      , emissionProbability = \ s e -> 1.0/logFloat (length eL)
                      }
                          where evenDist = 1.0 / sLlen
                                skewedDist s = logFloat (1+elemIndex2 s sL) / ( (sLlen * (sLlen +  logFloat (1.0 :: Double)))/2.0)
                                sLlen = logFloat $ length sL

-----------------------------------------------

--- use memoization to do dynamic programming

------------------------------------------------------------------
-- forward algorithm : -------------------------------------------
-- this is the forward part of the forward-backward algorithm
-- computes P( z_k | x_1..k)

-- P( (z_k) | x_1..k) = Sum over z_(k-1)	{	P(x_k|z_k)	*	P(z_k|z_k-1)	* P(z_k-1 | x_1..k-1)	}
-- P ( z_1 | x_1 ) = initProb (z_1) * emissionProb (z_1,x_1)

-- alpha (z_k,k) = Sum over z_(k-1)	{	emissionProb(x_k|z_k)	*	transitionProb(z_k|z_k-1)	* alpha(z_k-1,k-1)	}
-- alpha (z_1,1) = initProb (z_1) * emissionProb (z_1,x_1)

--reference : http://www.youtube.com/watch?v=M7afek1nEKM


forwardAlgorithmRecursive :: (Show stateType, Eq stateType, Show observationType, Eq observationType) => HMM stateType observationType -> stateType -> [observationType] -> ProbType

forwardAlgorithmRecursive hmm s obs =
	alpha s n
	where
		n = length obs
		obs1 = (listArray (1,n) obs)
		alpha z 1 = (startProbability hmm z) * (emissionProbability hmm z $ obs1!1 )
		alpha z k = (emissionProbability hmm z $ obs1!k ) * (sum $ [(transitionProbability hmm s z)*(alpha s (k-1)) | s <- states hmm])


forwardAlgorithm :: (Show stateType, Eq stateType, Show observationType, Eq observationType) => HMM stateType observationType -> stateType -> [observationType] -> ProbType

forwardAlgorithm hmm s obs =
	alpha (stateIndex hmm s) n 		--we pass stateindex as memoization on integers only
	where
		n = length obs
		obs1 = (listArray (1,n) obs)
		alpha = (Memo.memo2 Memo.integral Memo.integral alpha_dp)

		alpha_dp z k 							--z,k are integers here !!
			| k==1 	= (startProbability hmm (states hmm !! z)) * (emissionProbability hmm (states hmm !! z) $ obs1!1 )

			| otherwise = (emissionProbability hmm (states hmm !! z) $ obs1!k ) * (sum $ [(transitionProbability hmm s (states hmm !! z))*(alpha (stateIndex hmm s) (k-1)) | s <- states hmm])

-------------------------------------------------------------------
-- backward algorithm : -------------------------------------------
-- this is the backward part of the forward-backward algorithm
-- computes P (x_k+1 ... x_n | z_k)

-- P (x_k+1 ... x_n | z_k) = Sum over z_(k-1)	{ 	P (z_k+1 | z_k)	*	P(x_k+1|z_k+1)	*	P(x_k+2 ... x_n | z_k+1)	}

-- beta(z_k,k) = Sum over z_(k+1) {	beta(z_k+1,k+1)	*	emissionProb(x_k+1|z_k+1)	*	transitionProb(z_k+1 | z_k)	}
-- beta (z_n,n) = 1

-- reference : http://www.youtube.com/watch?v=jwYuki9GgJo

backwardAlgorithmRecursive :: (Show stateType, Eq stateType, Show observationType, Eq observationType) => HMM stateType observationType -> stateType -> [observationType] -> ProbType

backwardAlgorithmRecursive hmm z obs =
	beta z 1
	where 
		n = length obs
		obs1 = (listArray (1,n) obs)
		beta z k
			| k==n = 1
			| otherwise = sum [ (beta s (k+1)) * (emissionProbability hmm s $ obs1!(k+1) ) * (transitionProbability hmm z s) | s <- states hmm ]


backwardAlgorithm :: (Show stateType, Eq stateType, Show observationType, Eq observationType) => HMM stateType observationType -> stateType -> [observationType] -> ProbType

backwardAlgorithm hmm z obs =
	beta (stateIndex hmm z) 1 				--here, beta takes integers as input because of memoization
	where
		n = length obs
		obs1 = (listArray (1,n) obs)
		beta = (Memo.memo2 Memo.integral Memo.integral beta_dp)
		beta_dp z k
			| k==n = 1
			| otherwise = sum [ (beta (stateIndex hmm s) (k+1)) * (emissionProbability hmm s $ obs1!(k+1) ) * (transitionProbability hmm (states hmm !! z) s) | s <- states hmm ]


---------------------------------------------------------------------
--forward-backward algorithm-----------------------------------------

-- fbalgo (z,k,x_1 ... x_k) = Prob (z_k | x_1 ... x_k)

forwardBackwardAlgorithm hmm z k obs =
	prob_z / prob_total
	where
		(l1,l2) = splitAt k obs
		prob_z = (forwardAlgorithm hmm z l1) * (backwardAlgorithm hmm z l2)
		prob_total = sum [(forwardAlgorithm hmm s l1) * (backwardAlgorithm hmm s l2) | s <- states hmm]

---------------------------------------------------------------------
--viterbi algorithm : -----------------------------------------------

-- mu -> probability of most likely seq ending in z_k
-- mu(z_k) = max (z_k-1)	{	emissionProb(x_k|z_k)	*	transitionProb(z_k|z_k-1)	*	mu(z_k-1)	}	[k>1]
-- mu(z_1) = initProb (z_1) * emissionProb (z_1,x_1)

-- reference : http://www.youtube.com/watch?v=t3JIk3Jgifs

viterbiAlgorithm :: (Show stateType, Eq stateType, Show observationType, Eq observationType) => HMM stateType observationType -> [observationType] -> [stateType]

viterbiAlgorithm hmm obs =
	reverse $ giveLikelySequence z_n n
	where 
		n = length obs
		m = length (states hmm)
		obs1 = (listArray (1,n) obs)
		mu = (Memo.memo2 Memo.integral Memo.integral mu_dp)
		mu_dp z k
			| k==1 = (startProbability hmm (states hmm !! z) ) * (emissionProbability hmm (states hmm !! z) $ obs1!(k) )
			| otherwise = (emissionProbability hmm (states hmm !! z) $ obs1!(k)) * maximum [ (mu (stateIndex hmm s) (k-1)) * (transitionProbability hmm s (states hmm !! z)) | s <- states hmm]

		giveLikelySequence z k
			| k==1 = [states hmm !! z]
			| otherwise = (states hmm !! z) : (giveLikelySequence z' (k-1)) 
			where
				z' = argmax (\i -> (mu i (k-1)) * (transitionProbability hmm (states hmm !! i) (states hmm !! z)) ) [0..(m-1)]

		z_n = argmax (\i -> mu i n) [0..(m-1)]

--------------------------------------------------------------------
--------------------------------------------------------------------

-- given an observation sequence, baumWelch learns HMM parameters based on expectation maximization
--Thus, baumwelch gives 

--- the code is based on the following reference : http://tcs.rwth-aachen.de/lehre/PRICS/WS2006/kohlschein.pdf ---

baumWelchAlgorithm :: (Show stateType, Eq stateType, Show observationType, Eq observationType) => HMM stateType observationType -> [observationType] -> Int -> HMM stateType observationType

baumWelchAlgorithm hmm obs numIter
	| numIter == 0 = hmm
	| otherwise =
		trace (show hmmNew) $
		baumWelchAlgorithm hmmNew obs (numIter-1)
	where 
		hmmNew = baumWelchIteration hmm obs

baumWelchIteration :: (Show stateType, Eq stateType, Show observationType, Eq observationType) => HMM stateType observationType -> [observationType] -> HMM stateType observationType

baumWelchIteration hmm obs =

	--------returs an updated HMM---------------
	--------------------------------------------
	HMM {states = states hmm
	, observations = observations hmm
	, startProbability = startProbabilityNew
	, transitionProbability = transitionProbabilityNew
	, emissionProbability = emissionProbabilityNew}

	where
		n = length obs
		obs1 = (listArray (1,n) obs)


		------------------------------------------------------------------
		-----------alpha , beta are same as above so not explaining here--
		alpha = (Memo.memo2 Memo.integral Memo.integral alpha_dp)
		alpha_dp z k 							--z,k are integers here !!
			| k==1 	= (startProbability hmm (states hmm !! z)) * (emissionProbability hmm (states hmm !! z) $ obs1!1 )
			| otherwise = (emissionProbability hmm (states hmm !! z) $ obs1!k ) * (sum $ [(transitionProbability hmm s (states hmm !! z))*(alpha (stateIndex hmm s) (k-1)) | s <- states hmm])


		beta = (Memo.memo2 Memo.integral Memo.integral beta_dp)
		beta_dp z k 							--z,k are integers here !!
			| k==n = 1
			| otherwise = sum [ (beta (stateIndex hmm s) (k+1)) * (emissionProbability hmm s $ obs1!(k+1) ) * (transitionProbability hmm (states hmm !! z) s) | s <- states hmm ]

		------------------------------------------------------------------
		------------------------------------------------------------------

		--prob_obseq is prob of seeing the sequence of observations, same as p(V^T | theta)
		--sum s  { beta_s * initprob(s) * emitprob(s,x1)}
		prob_obseq = sum [(startProbability hmm s) * (emissionProbability hmm s $ obs1!1) * (beta (stateIndex hmm s) 1) | s <- states hmm ]


		-- gamma as defined in the pseudocode
		-- gamma i j t give the probility that we had s_i at (t-1) which had a transition to s_j at t
		gamma i j t = (alpha i (t-1)) * (transitionProbability hmm s_i s_j) * (emissionProbability hmm s_j o_t) * (beta j t) / prob_obseq
			where
				s_i = (states hmm !! i)
				s_j = (states hmm !! j)
				o_t = (obs1 ! t)

		-- sumgamma i t gives the probability of having s_i at t-1
		sumGamma i t = sum [gamma i (stateIndex hmm s) t | s <- states hmm]

		-- transition probility s_i -> s_j = (expected number of times we have s_i -> s_j transit ) / (expected number of times we have s_i)
		transitionProbabilityNew s_i s_j =
			numerator / denominator
			where
				i = stateIndex hmm s_i
				j = stateIndex hmm s_j
				numerator = sum [gamma i j t | t <- [2..n]]
				denominator = sum [sumGamma i t | t <- [2..n]]

		-- emission probability s_i -> o = (expected number of times we have s_i -> o transit ) / (expected number of times we have s_i)
		emissionProbabilityNew s_i o =
			numerator / denominator
			where
				i = stateIndex hmm s_i
				numerator = sum [if ((obs1!t) == o) then sumGamma i t else 0 | t <- [2..n] ]
				denominator = sum [sumGamma i t | t <- [2..n]]

		-- start probability = sumgamma 2 as it gives prob of having s_i at t=1
		startProbabilityNew s_i = 
			sumGamma (stateIndex hmm s_i) 2

--------------------------------------------------------------------
main = do
	let slist = ["rainy","sunny"]
	let olist = ["walk","shop","clean"]
	let hmm = simpleHMM slist olist
	print $ show hmm
	putStrLn ""
	putStrLn "Forward Algo Recursive started -"
	print $ forwardAlgorithmRecursive hmm "rainy" ["shop","clean","walk"]
	putStrLn ""
	putStrLn "Forward Algo started -"
	print $ forwardAlgorithm hmm "rainy" ["shop","clean","walk"]
	putStrLn ""
	putStrLn "Backward Algo Recursive started -"
	print $ backwardAlgorithmRecursive hmm "rainy" ["shop","clean","walk"]
	putStrLn ""
	putStrLn "Backward Algo started -"
	print $ backwardAlgorithm hmm "rainy" ["shop","clean","walk"]
	putStrLn ""
	putStrLn "Viterbi Algo started -"
	print $ viterbiAlgorithm hmm ["shop","clean","walk"]

	let trainseq = ["shop","clean","walk","shop","clean","walk","shop","clean","walk","shop","clean","walk","shop","clean","walk"]
	let hmmNew = baumWelchAlgorithm hmm trainseq 3
	putStrLn ""
	putStrLn ""
	putStrLn "Show HMM New -"
	print $ show hmmNew
