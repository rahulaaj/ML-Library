type Input = [Double]
type Weights = [Double]
type Bias = Double
type BinClass = Double
type LearnRate = Double

newtype Neuron = (Input, Weights, Bias)

output :: Neuron -> BinClass
output (inp, wts, th)	| total >= th	= 1
			| otherwise	= -1
	where total = foldr (+) 0 (zipWith (*) inp wts) 

trainWeights :: Input -> Weights -> LearnRate -> BinClass -> BinClass -> Weights
trainWeights inp iniWts lr expClass actualClass = map wChngeFunc (zip inp iniWts)
	where wChngeFunc (x, w) = w + lr * (actualClass - expClass) * x