import System.IO
import Data.List
import Control.Monad
import Data.Text
import System.IO.Unsafe 

num_iter=2
readI::String ->Int
readI=read
readIList []=[]
readIList x=Data.List.map (readI . unpack) (splitOn (pack "\t") (pack x))

readD::String ->Double
readD=read
readDList []=[]
readDList x=Data.List.map (readD . unpack) (splitOn (pack "\t") (pack x))

getData []=[]
getData (x:xs)=[(readDList x!!0,[(show $ readDList x!!2, readDList x!!1)])]++getData xs
readData file = do 
                handle <- openFile file ReadMode
                contents <- hGetContents handle
                let !list = Data.List.lines contents
                --print "Input Data"
                --print list
                hClose handle
                return (getData list)

{-
main= do
        x<-readData "logisticRegression.txt"
        print x
-}
dataF=unsafePerformIO $ readData "logisticRegression.txt"
--main=print dataF
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
--weight=[]
addToOld [] k1 val=[]
addToOld ((k,v):xs) k1 val|(k1==k)=[(k,v+val)]++ xs
                          | otherwise=[(k,v)]++ addToOld xs k1 val

add dict k new=dict++[(k,new)]

lookUp k []=False
lookUp k ((k1,v):xs)|(k1==k) = True
                    | otherwise = lookUp k xs

findVal k []=0
findVal k ((k1,v):xs)|(k1==k) = v
                     | otherwise = findVal k xs

someIterate weight []=0
someIterate weight ((k,v):xs)|lookUp k weight = 0+(findVal k weight)*v+ someIterate weight xs
                         | otherwise = 0+someIterate weight xs

classify feature weight= 1.0/(1.0+exp(-logit)) where
                logit= someIterate weight feature

someIterateTrain [] x weight = return weight
someIterateTrain ((k,v):xs) x weight|(lookUp k weight)= someIterateTrain xs x (addToOld weight k new) where
                                                                new=rate*(x-(classify ((k,v):xs) weight))*v
someIterateTrain ((k,v):xs) x weight| otherwise=someIterateTrain xs x (add weight k (rate* update)) where
                                                        update= (x-(classify ((k,v):xs) weight))*v

train 0 _ weight=return weight
train n [] weight=return weight
train n ((x,feature):xs) weight = do newWeight<-someIterateTrain feature x weight;
                                     --print newWeight
                                     nextWeight<-train n xs newWeight;
                                     --print newWeight
                                     --print ("Done "++(show n));
                                     train (n-1) ((x,feature):xs) nextWeight;
printList []=print "Regression Over"
printList ((d,i):xs)= do
                        print ("Probabilistic Weight of class"++(show d)++" is "++(show i))
                        printList xs

main = do x<-(train num_iter dataF ([]))
          printList x
