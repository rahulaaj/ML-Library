import System.IO
import Data.List
import GHC.Show
{-
fac 1 = 1
fac n = n * (fac (n-1))
main = print (fac 5)
-}
{-
fac n = if (n==1) then 1
        else n * fac(n-1)
--main = print (fac 6)
-}
{-
imap f [] = []
imap f (x:xs) = f x: imap f xs
main = print (imap fac [1..10])
-}
{-
length1 [] = 0
length1 (x:xs) = 1 + length1 xs
main = print (length1 [1..10])
-}
{-
empty [] = True
empty _ = False
--main = print (empty [])
--main = print (empty ['1','2'])
--main = print (empty 'x') 
-}
{-
x a b|(a>0) = 2
     |(b>0) = 3
main = print (x (-1) 2)
-}
{-
--temp " "=[]
temp (x:xs)|(xs == ' ')=[x]
           | otherwise=[x]++temp xs
main = print (temp "hieie")
-}
{-
iwords ""=[""]
iwords (x:xs)|(x ==' ') = "":(iwords xs)
             |otherwise = (x:head (iwords xs)):tail (iwords xs)
--main = print (iwords "hi hello whatsup")
--a = [1,2,3]
--a = a ++ ([6,7])
--main = print (a)
-}
{-
iunwords []=""
iunwords [x]=x
iunwords (x:xs)=x++(" ")++(iunwords xs)
--main = print (iunwords (["hi","hello","ok"]))
-}
--temp :: [String]->[String]
{-
temp [""]=[""]
temp (x:xs)|(length xs)>1=[x,","]++(temp xs)
           |otherwise=[x,"and",xs !! 0]
--main = print (temp ["hi","hello","oo"])
punctuate ""=""
punctuate x=(iunwords . temp . iwords) x
main = print (punctuate ("hi hello ok"))
-}
{-
type Point=(Double,Double)
temp::Point->Double
temp a=fst a + snd a
main = print (temp (1,2))
-}
{-
a=(1,2,3)
--temp [] = 0
temp (x1,x2,x3)=x1+x2+x3
main = print (temp a)
-}
{-
safeHead (x:xs)=Just x
safeHead _=Nothing
main = print (safeHead ([1,2]))
-}
{-
getInt :: IO Int
getInt = readLn
fac 1 = 1
fac n = n * (fac (n-1))
facPrint = print.fac
main = (getInt >>= facPrint)
-}
{-
getInt :: IO Int
getInt = readLn
nmod x|x>0 = x
      |otherwise = 0-x
main = do putStrLn("Enter a number")
          n <- getInt
          m <- getInt
          print(nmod (n+m))
-}
{-
--does not works!!
modsum x1 x2 = do v1 <- mod x1
                  v2 <- mod x2
                  return (v1+v2)
main = print (modsum (-2) 3)
-}
power x n
    | n == 0  =  1
    | x == 0  =  0
    | (even n) == True = ( power x (n `div` 2) ) * ( power x (n `div` 2) )
    | (odd n) == True = x * ( power x ((n - 1) `div` 2)) * ( power x ((n - 1) `div` 2) )
main = print (power 3 2)
{-
fib 0=0
fib 1=1
fib x = (fib (x-1)) + (fib (x-2))
main = print (fib 5)
-}
