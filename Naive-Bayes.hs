import Data.List
import Data.Char
data Classifier = Classifier {
		wordCategoryStore :: [(String,[(String,Int)])]
	}

--returns total number of words of the category
categoryCount :: (Eq t, Num a) => [(t, a)] -> t -> a
categoryCount ws category = sum $ map g $ filter f ws
	where
		f (a,b) = a==category
		g (a,b) = b

--returns number of occurances of word in the category
wordCategoryCount :: (Eq a, Eq a1, Num a2) => [(a1, [(a, a2)])] -> a1 -> a -> a2
wordCategoryCount ws category word = sum $ map f ws
	where
		f (x,y) = if (x==category)  then (g y) else 0
		g [] = 0
		g ((a,b):xs) = if (a==word) then b else g xs

--trains the classifier by adding doc with respective category
train :: Classifier -> String -> String -> Classifier
train (Classifier ws) category doc = Classifier cs
	where
		cs= (category, pairs) : ws
		pairs = map (\x-> (head x, length x)) $ group $ sort $ words doc

--total number of words in classifier
totalCount :: Num [b] => [(t, [(t1, b)])] -> [b]
totalCount cs = sum $ map f cs
	where
		f(a,b)=map g b
		g(x,y) = y

--returns all categories
categories :: [(b, t)] -> [b]
categories cs = map f cs
	where
		f (a,b)=a

--return the probability of the word being in a particular category
probWordCategory :: (Eq t, Eq a2, Fractional a, Integral a1, Integral [(a2, a1)]) => [(t, [(a2, a1)])] -> t -> a2 -> a
probWordCategory cs category word =( fromIntegral $ wordCategoryCount cs category word ) / (fromIntegral $ categoryCount cs category)

--return the probability of categry
probCategory :: (Eq t, Fractional a, Integral [(t1, b)], Integral [b]) =>[(t, [(t1, b)])] -> t -> a
probCategory cs category = (fromIntegral $ categoryCount cs category) / ( fromIntegral $ totalCount cs )

--probability of each cateogry
probAllCategories :: (Eq a, Fractional b, Integral [b1], Integral [(t, b1)]) => [(a, [(t, b1)])] -> [b]
probAllCategories cs = map (probCategory cs) $ map fst cs

--probability of document to be in a catgory
probDocCategory :: (Eq t, Fractional a, Integral [a1], Integral [(String, a1)], Integral a1) => [(t, [(String, a1)])] -> String -> t -> a
probDocCategory cs doc category =probCategory cs category * ( foldl (*) 1.0 $ map (probWordCategory cs category) $ words doc )
