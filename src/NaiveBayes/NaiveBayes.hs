module NaiveBayes.NaiveBayes where

	import Data.List
	import Data.Char
	data Classifier = Classifier {
			wordCategoryStore :: [(String,[(String,Int)])]
		}

	stopwords = ["a",
    		"about",
	    	"above",
    		"after",
    		"again",
    		"against",
    		"all",
    		"am",
    		"an",
    		"and",
	    	"any",
    		"are",
    		"arent",
    		"aren't",
    		"as",
    		"at",
    		"be",
    		"because",
    		"been",
    		"before",
    		"being",
    		"below",
    		"between",
    		"both",
    		"but",
    		"by",
    		"cant",
    		"can't",
    		"cannot",
    		"could",
    		"couldnt",
    		"couldn't",
    		"did",
    		"didnt",
    		"didn't",
    		"do",
    		"does",
    		"doesnt",
    		"doesn't",
    		"doing",
    		"dont",
    		"don't",
    		"down",
    		"during",
    		"each",
    		"few",
    		"for",
    		"from",
    		"further",
    		"had",
    		"hadnt",
    		"hadn't",
    		"has",
    		"hasnt",
    		"hasn't",
    		"have",
    		"havent",
    		"haven't",
    		"having",
    		"he",
    		"hed",
    		"he'd",
    		"he'll",
    		"hes",
    		"he's",
    		"her",
    		"here",
    		"heres",
    		"here's",
    		"hers",
    		"herself",
    		"him",
    		"himself",
    		"his",
    		"how",
    		"hows",
    		"how's",
    		"i",
    		"i'd",
    		"i'll",
    		"im",
    		"i'm",
    		"ive",
    		"i've",
    		"if",
    		"in",
    		"into",
    		"is",
    		"isnt",
    		"isn't",
    		"it",
    		"it's",
    		"its",
    		"itself",
    		"lets",
    		"let's",
    		"me",
    		"more",
    		"most",
    		"mustnt",
    		"mustn't",
    		"my",
    		"myself",
    		"no",
    		"nor",
    		"not",
    		"of",
    		"off",
    		"on",
    		"once",
    		"only",
    		"or",
    		"other",
    		"ought",
    		"our",
    		"ours ",
    		"ourselves",
    		"out",
    		"over",
    		"own",
    		"same",
    		"shant",
    		"shan't",
    		"she",
    		"she'd",
    		"she'll",
    		"shes",
    		"she's",
    		"should",
    		"shouldnt",
    		"shouldn't",
    		"so",
    		"some",
    		"such",
    		"than",
    		"that",
    		"thats",
    		"that's",
    		"the",
    		"their",
    		"theirs",
    		"them",
    		"themselves",
    		"then",
    		"there",
    		"theres",
    		"there's",
    		"these",
    		"they",
    		"theyd",
    		"they'd",
    		"theyll",
    		"they'll",
    		"theyre",
    		"they're",
    		"theyve",
    		"they've",
    		"this",
    		"those",
    		"through",
    		"to",
    		"too",
    		"under",
    		"until",
    		"up",
    		"very",
    		"was",
    		"wasnt",
    		"wasn't",
    		"we",
    		"we'd",
    		"we'll",
    		"we're",
    		"weve",
    		"we've",
    		"were",
    		"werent",
    		"weren't",
    		"what",
    		"whats",
    		"what's",
    		"when",
    		"whens",
    		"when's",
    		"where",
    		"wheres",
    		"where's",
    		"which",
    		"while",
    		"who",
    		"whos",
    		"who's",
    		"whom",
    		"why",
    		"whys",
    		"why's",
    		"with",
    		"wont",
    		"won't",
    		"would",
    		"wouldnt",
    		"wouldn't",
    		"you",
    		"youd",
    		"you'd",
    		"youll",
    		"you'll",
    		"youre",
    		"you're",
    		"youve",
    		"you've",
    		"your",
    		"yours",
    		"yourself",
    		"yourselves"]
	
	--removeStopWords from doc
	removeStopwords ::String-> [String]
	removeStopwords doc = filter g $ words doc
	  where
    	g x = notElem x stopwords

	--returns total number of words of the category
	categoryCount :: [(String,[(String,Int)])] -> String -> Int
	categoryCount ws category =
		let
			f (a,b) = a==category
			l = filter f ws
			g (p,q) =q
			h = map g l
			i = concat h
			j = map snd i
		in
			sum j
	
	--returns number of occurances of word in the category
	wordCategoryCount :: [(String,[(String,Int)])] -> String -> String -> Int
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
	totalCount :: [(String,[(String,Int)])] -> Int
	totalCount cs =
		let 
			f (a,b)= b
			g = map f cs
			h = concat g
			i (a,b) = b
			j = map i h
		in
			sum j
	
	--returns all categories
	categories :: [(String,[(String,Int)])] -> [String]
	categories cs = nub $ map f cs
		where
			f (a,b)=a
	
	--return the probability of the word being in a particular category
	probWordCategory :: [(String,[(String,Int)])] -> String -> String -> Double
	probWordCategory cs category word =
		let
			allWords = fromIntegral $ categoryCount cs category
			wordCount = fromIntegral $ wordCategoryCount cs category word
		in
			wordCount / allWords

	--return the probability of categry
	probCategory :: [(String,[(String,Int)])] -> String -> Double
	probCategory cs category = (fromIntegral $ categoryCount cs category) / ( fromIntegral $ totalCount cs )
	
	--probability of each cateogry
	probAllCategories :: [(String,[(String,Int)])] -> [Double]
	probAllCategories cs = map (probCategory cs) $ map fst cs
	
	--probability of document to be in a catgory
	probDocCategory :: [(String,[(String,Int)])] -> String -> String -> Double
	probDocCategory cs doc category =probCategory cs category * ( foldl (*) 1.0 $ map (probWordCategory cs category) $ words doc )

	--probability of document in all categories
	probDocAllCategories :: [(String,[(String,Int)])] -> String -> [(String,Double)]
	probDocAllCategories cs doc = map (\cat -> (cat,probDocCategory cs doc cat)) $ categories cs
	
	--classify document
	classify :: Classifier -> String -> (String,Double)
	classify cls doc = 
		let
			l = probDocAllCategories (wordCategoryStore cls ) doc
			f (a,b) (p,q) = q `compare` b
			h = sortBy f l
			i = head h
		in
			i
