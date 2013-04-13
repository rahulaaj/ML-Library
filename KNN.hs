import Data.List

dist :: [Double]->[Double]->Double
dist p q = sqrt $ 	sum $ zipWith (\a b-> (a-b)*(a-b)) p q

takeKNeighbours :: Int -> [(Double,(String,[Double]))]->[(Double,(String,[Double]))]
takeKNeighbours k distances = take k $ sortBy f distances
	where
		f (a,b) (p,q) 
			| a<=p = LT
			| a>p = GT

findDistances :: [(String,[Double])]->[Double]->[(Double,(String,[Double]))]			
findDistances points p = map f points
	where
		f (a,b) = (dist p b, (a,b))
		
knn :: [(String,[Double])]->[Double]->Int->String
knn model test k =
	let
		distances = findDistances model test
		cluster = takeKNeighbours k distances
		points = map snd cluster
		labels = map fst points
		clusterLabels = group labels
		numLabels = map (\x -> (length x,head x)) clusterLabels
		bestClass = maximumBy f numLabels
		f (a,b) (p,q)
			| a>=p = GT
			| a<p = LT
		result = snd bestClass
	in result
