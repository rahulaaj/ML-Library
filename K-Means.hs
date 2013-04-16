import Data.List
import Data.Ord
import Data.Char

module KMeans where

	--find the number of points
	number :: [[Double]]->Int
	number points= length points

	--find the sum component wise 
	sumVectors :: [[Double]]->[Double]
	sumVectors points = map sum $ transpose points
	  
	--find the centroid of a collection of points
	centroid :: [[Double]]->[Double]
	centroid points = map f $ sumVectors points 
	  where
	    f a = a / ( fromIntegral $ number points)
	    
	--find distance between 2 points
	dist :: [Double]->[Double]-> Double
	dist x y = sqrt $ sum $ zipWith f x y
	  where
	    f a b=(a-b)*(a-b)
	    
	--find the minimum distance between a ref point and the collection points
	minDist::[Double]->[[Double]]->(Double,[Double])
	minDist refPoint points = f $ distAll refPoint points
	  where
		f (x:[]) = x
		f ((a,b):xs) = if a < ( fst $ minDist refPoint $ map snd xs)
						then (a,b)
						else minDist refPoint $ map snd xs
		
	distAll :: [Double]->[[Double]]->[(Double,[Double])]
	distAll refPoint points= map g points
	  where
		g a=(dist refPoint a, a)
	
	--find the centroid of each point
	findCentroid :: [[Double]]->[[Double]]->[([Double],[Double])]
	findCentroid centroids points = map (\x -> (snd $ minDist x centroids,x)) points
	
	--make cluster given centroids and points
	makeCluster :: [[Double]]->[[Double]]->[[[Double]]]
	makeCluster centroids points =map (map snd) $ groupBy f (findCentroid centroids points)
	  where
	    f (a,b) (c,d) = a==c
	    
	--divide the list of points into clusters of k size
	dividePoints :: Int->[[Double]]->[[[Double]]]
	dividePoints k points 
	  | second==[] = [first]
	  | otherwise = first : (dividePoints l second)
	  where
	    (first,second)=splitAt l points
	    l = (length points + k - 1) `div` k
	
	--initates cluster formation
	clustering :: [[[Double]]]->[[[Double]]]
	clustering clusters 
	  | newClusters==clusters = clusters
	  | otherwise = clustering newClusters
	  where
	    newClusters = makeCluster centroids points
	    centroids = map centroid clusters
	    points = concat clusters
	  
	--returns the clusters
	kmeans :: Int->[[Double]]->[[[Double]]]
	kmeans k points = clustering $ dividePoints k points	
