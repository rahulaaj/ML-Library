13.2  Array Creation

Haskell's monolithic array creation function forms an array from a pair of bounds and a list of index-value pairs (an association list): 

array                   :: (Ix a) => (a,a) -> [(a,b)] -> Array a b

Here, for example, is a definition of an array of the squares of numbers from 1 to 100: 

squares                 =  array (1,100) [(i, i*i) | i <- [1..100]]

This array expression is typical in using a list comprehension for the association list; in fact, this usage results in array expressions much like the array comprehensions of the language Id [6].
Array subscripting is performed with the infix operator !, and the bounds of an array can be extracted with the function bounds:

squares!7 => 49

bounds squares => (1,100)

We might generalize this example by parameterizing the bounds and the function to be applied to each index: 

mkArray                 :: (Ix a) => (a -> b) -> (a,a) -> Array a b
mkArray f bnds          =  array bnds [(i, f i) | i <- range bnds]

Thus, we could define squares as mkArray (\i -> i * i) (1,100).

Many arrays are defined recursively; that is, with the values of some elements depending on the values of others. Here, for example, we have a function returning an array of Fibonacci numbers: 

fibs    :: Int -> Array Int Int
fibs n  =  a  where a = array (0,n) ([(0, 1), (1, 1)] ++ 
                                     [(i, a!(i-2) + a!(i-1)) | i <- [2..n]])

Another example of such a recurrence is the n by n wavefront matrix, in which elements of the first row and first column all have the value 1 and other elements are sums of their neighbors to the west, northwest, and north: 

wavefront       :: Int -> Array (Int,Int) Int
wavefront n     =  a  where
                   a = array ((1,1),(n,n))
                        ([((1,j), 1) | j <- [1..n]] ++
                         [((i,1), 1) | i <- [2..n]] ++
                         [((i,j), a!(i,j-1) + a!(i-1,j-1) + a!(i-1,j))
                                     | i <- [2..n], j <- [2..n]])

The wavefront matrix is so called because in a parallel implementation, the recurrence dictates that the computation can begin with the first row and column in parallel and proceed as a wedge-shaped wave, traveling from northwest to southeast. It is important to note, however, that no order of computation is specified by the association list.

In each of our examples so far, we have given a unique association for each index of the array and only for the indices within the bounds of the array, and indeed, we must do this in general for an array be fully defined. An association with an out-of-bounds index results in an error; if an index is missing or appears more than once, however, there is no immediate error, but the value of the array at that index is then undefined, so that subscripting the array with such an index yields an error.

listarrays : refer haskell tutorial-
Arrays> listArray (1,5) [3,7,5,1,10]
array (1,5) [(1,3),(2,7),(3,5),(4,1),(5,10)]



