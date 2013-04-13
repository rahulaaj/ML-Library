import System.IO
import Data.List
import GHC.Show
import Numeric.LinearAlgebra.Algorithms 
import Data.Packed.Matrix
m=(3><3)[1,2,3,4,5,6,7,8,9]
m::Matrix Double
q::Matrix Double
r::Matrix Double
addOneElem []=[]
addOneElem x=[1]++x
addOne [[]]=[[]]
addOne (x:xs)=[addOneElem x] ++ addOne xs
insertOne m= fromLists $ addOne $ toLists m
(q,r)=qr $ insertOne m
--main=print (addOne ([[1,2,3],[2,4]]))
--main=print $ addOneElem [1,2,3]
main=print $ addOneElem [1,2,3]

