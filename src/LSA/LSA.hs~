module LSA where 

	import Data.List
	import Data.Char
	import Numeric.LinearAlgebra

	--["The Neatest Little Guide to Stock Market Investing","Investing For Dummies, 4th Edition","The Little Book of Common Sense Investing: The Only Way to 		Guarantee Your Fair Share of Stock Market Returns","The Little Book of Value Investing","Value Investing: From Graham to Buffett and Beyond","Rich Dad's Guide to 	Investing: What the Rich Invest in, That the Poor and the Middle Class Do Not!","Investing in Real Estate, 5th Edition","Stock Investing For Dummies","Rich Dad's Advisors: The ABC's of Real Estate Investing: The Secrets of Finding Hidden Profits Most Investors Miss" ]

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
		
	text ::[String]-> String
	text titles=map toLower $ unlines titles

	removeUnwanted ::[String]-> String
	removeUnwanted titles = filter f $ text titles
	  where
	    f x = isSpace x || isAlpha x
	    
	getLines ::[String]-> [String]
	getLines titles = lines $ removeUnwanted titles
	
	getWords ::[String]-> [[String]]
	getWords titles = map words $ getLines titles
	
	removeStopwords ::[String]-> [[String]]
	removeStopwords titles = map (filter g)  $ getWords titles
	  where
	    g x = notElem x stopwords
	    
	getDoc :: [String]->Int->[String]
	getDoc titles doc = removeStopwords titles !! doc
	
	getVocab :: [String]->[String]
	getVocab titles = concat $ removeStopwords titles
	
	termFrequency :: [String]->[(String,Int)]
	termFrequency titles = map (\x-> (head x, length x)) $ group $ sort $ getVocab titles
	
	tf :: [String]->[(String,Int)]
	tf titles = filter f $ termFrequency titles
		where
			f (a,b) = b>=1
	
	termFreqDoc :: [String]->Int->String->Int
	termFreqDoc titles doc term = length $ filter (==term) $ getDoc titles doc
	
	tdm :: [String]->Matrix Double
	tdm titles = buildMatrix (length $ tf titles) (length $ removeStopwords titles) ( \(term,doc)->
		let
			a = tf titles !! term
			b = fst a
			c = termFreqDoc titles doc b
			d = getDoc titles doc
			termDocFreq = ( fromIntegral c ) / ( genericLength d)
			e = removeStopwords titles
			f = tf titles !! term
			g = snd f
			idf = (genericLength e) / ( fromIntegral g)
		in termDocFreq * log(idf)) 
	
	lsa :: [String]->Int->(Matrix Double,Matrix Double,Matrix Double)
	lsa titles k =
		let
			(u,sigma,v)=fullSVD $ tdm titles
			uk = takeColumns k u
			vk = takeRows k $ trans v
			sigmak = takeColumns k $ takeRows k sigma
		in 
			(uk,sigmak,vk)
	
