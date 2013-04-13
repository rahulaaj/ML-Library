import Data.List
--emptyDict= const Nothing
--add :: Eq a => (a -> Maybe b) -> a -> b -> (a -> Maybe b) 
--add dict key value = \x -> if x == key
--                                then Just value
--                                else dict x
--remove dict key = \x -> if x == key
--                                then Nothing
--                                else dict x
type Feature=[(String, Double)]

--class LogisticRegression l where
--        rate :: l -> Double
--        weight :: l -> Feature
--        train :: l -> [(Int,Feature)] -> IO()
--        classify :: l -> Feature -> Double

--instance LogisticRegression l where
rate=0.01
weight=[]
lookUp k []=False
lookUp k ((k1,v):xs)|(k1==k) = True
                    | otherwise = lookUp k xs
findVal k []=0
findVal k ((k1,v):xs)|(k1==k) = v
                    | otherwise = findVal k xs
iterate weight []=
iterate weight ((k,v):xs)|lookUp k weight = 0+(findVal k weight)*v+ iterate weight xs
                         | otherwise = 0+iterate weight xs
classify feature= 1.0/(1.0+exp(-logit)) where
                logit= iterate weight feature
iterateTrain [] x weight = print "Done"
iterateTrain ((k,v):xs) x weight|lookUp k weight= iterateTrain xs x (addToOld weight k new) where
                                                                new=rate*(x-(classify ((k,v):xs)))*v
                                | otherwise=iterateTrain xs x (add weight k (rate* update)) where
                                                        update= (x-(classify ((k,v):xs)))*v
train 0 _=print "Complete"
train n []=print $ show n
train n ((x,feature):xs)= do iterateTrain feature x weight;
                             train n xs;
                             train (n-1) ((x,feature):xs);
