import System.IO
import Data.List
import GHC.Show
import Numeric.LinearAlgebra.Algorithms 
import Numeric.LinearAlgebra.LAPACK
import Data.Packed.Matrix
--m=(3><3)[1,2,3,4,5,6,7,8,9]
x=(20><6)[51,30,39,61,92,45,64,51,54,63,73,47,70,68,69,76,86,48,63,45,47,54,84,35,78,56,66,71,83,47,55,49,44,54,49,34,67,42,56,66,68,35,75,50,55,70,66,41,82,72,67,71,83,31,61,45,47,62,80,41,53,53,58,58,67,34,60,47,39,59,74,41,62,57,42,55,63,25,83,83,45,59,77,35,77,54,72,79,77,46,90,50,72,60,54,36,85,64,69,79,79,63,60,65,75,55,80,60,70,46,57,75,85,46,58,68,54,64,78,52]
y=(20><1)[43,63,71,61,81,43,58,71,72,67,64,67,69,68,77,81,74,65,65,50]
--m::Matrix Double
x::Matrix Double
y::Matrix Double
q::Matrix Double
r::Matrix Double
addOneElem []=[]
addOneElem x=[1.0]++x
addOne []=[]
addOne (x:xs)=[addOneElem x] ++ addOne xs
--main=print (addOne ([[1,2,3],[2,4]]))
insertOne m= fromLists $ addOne $ toLists m
(q,r)=qr $ insertOne x
n=cols r
--main=print (addOne ([[1,2,3],[2,4]]))
--main=print $ addOneElem [1,2,3]
transposeMat m = fromLists $ transpose $ toLists m
z=multiplyR (transposeMat q) y
--main=print z
--Solve Rb=z
--b= multiplyR (inv r) z
addToList j list bj=list++[(bj,j)]
findVal j []=0.0
findVal j ((b1,j1):xs)|(j==j1)=b1
                      |otherwise=findVal j xs
sumRB r b j j1 j2 list|(j2==j1)=0.0
sumRB r b j j1 j2 list=((@@>) r (j,j1))*(findVal j1 list)+ sumRB r b j (j1+1) j2 list
bSolve r z (-1) n list=list
bSolve r z j n list= bSolve r z (j-1) n newlist where
                                          bj=(((@@>) z (0,j))-(sumRB r b j (j+1) n list))/((@@>) r (j,j)); 
                                          newlist=addToList j list bj
b=bSolve r (transposeMat z) (n-1) n []
--b=linearSolve r z
main=do 
        print n        
        print b

