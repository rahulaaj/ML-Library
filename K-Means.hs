import Data.List
import Data.Ord

--find the number of points
number :: [[Double]]->Int
number points= length points

sumVectors :: [[Double]]->[Double]
sumVectors points = map sum $ transpose points
  
centroid :: [[Double]]->[Double]
centroid points = map f $ sumVectors points 
  where
    f a = a / ( fromIntegral $ number points)
    
dist :: [Double]->[Double]-> Double
dist x y = sqrt $ sum $ zipWith f x y
  where
    f a b=(a-b)*(a-b)
    
minDist :: [Double]->[[Double]]->[Double]
minDist refPoint points = minimumBy (comparing $ dist refPoint) points

findCentroid :: [[Double]]->[[Double]]->[([Double],[Double])]
findCentroid centroids points = map (\x -> (minDist x centroids,x)) points

makeCluster :: [[Double]]->[[Double]]->[[[Double]]]
makeCluster centroids points =map (map snd) $ groupBy f (findCentroid centroids points)
  where
    f (a,b) (c,d) = a==c
    
dividePoints :: Int->[[Double]]->[[[Double]]]
dividePoints k points 
  | second==[] = [first]
  | otherwise = first : (dividePoints l second)
  where
    (first,second)=splitAt l points
    l = (length points + k - 1) `div` k

clustering :: [[[Double]]]->[[[Double]]]
clustering clusters 
  | newClusters==clusters = clusters
  | otherwise = clustering newClusters
  where
    newClusters = makeCluster centroids points
    centroids = map centroid clusters
    points = concat clusters
  
kmeans :: Int->[[Double]]->[[[Double]]]
kmeans k points = clustering $ dividePoints k points
