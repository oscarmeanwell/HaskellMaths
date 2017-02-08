-- QUESTION 1 
allCombs xs = [2] >>= \n -> mapM (const xs) [1..n]

isReflexive :: (Eq a) => [(a,a)] -> Bool
isReflexive xs = (inSet (concat(remD [[p,o]|(p,o) <- xs])) (concat(remD [[q,w]|(q,w)<- [z|z<-xs, fst z == snd z]]))) && length xs > 0

isSymmetric :: (Eq a) => [(a,a)] -> Bool
isSymmetric xs = (length([c|c <- [[q,w]|(q,w)<- [z|z<-xs]], (elem (reverse c) [[q,w]|(q,w)<- [z|z<-xs]])])) == length [[q,w]|(q,w)<- [z|z<-xs]] && length xs > 0 


isTransitive :: (Eq a) => [(a,a)] -> Bool
isTransitive xs = ([[x,y]|[x,y] <- allCombs(remD(concat([[p,o]|(p,o) <- xs, p /= o]))), x /= y]) == ([[x,y]|(x,y) <-xs, x /= y]) && length xs > 0

isEquivalence :: (Eq a) => [(a,a)] -> Bool
isEquivalence xs = isReflexive xs && isSymmetric xs && isTransitive xs && length xs > 0

eqClassOf :: (Eq a) => [(a,a)] -> a -> [a]
eqClassOf xs r = remD(concat([[p,o]|(p,o) <- xs, elem r [p,o]]))

-- QUESTION 2

remD :: Eq a => [a] -> [a]
remD [] = []
remD (x:xs) = x : remD (filter (\y -> not (x == y)) xs)

inSet :: (Eq a) => [a] -> [a] -> Bool
inSet [] _ = True
inSet (x:xs) ys
	| elem x ys = inSet xs ys
	| otherwise = False

	
multiEqual :: (Eq a) => [a] -> [a] -> Bool
multiEqual xs ys = inSet xs ys && inSet ys xs && length xs == length ys

multiUnion :: (Eq a) => [a] -> [a] -> [a]
multiUnion [] _= []
multiUnion xs ys = remD xs ++ remD ys

multiIntersection :: (Eq a) => [a] -> [a] -> [a]
multiIntersection [] _ = []
multiIntersection xs ys = remD [z| z<-xs, elem z ys]

-- QUESTION 3
--trace :: (Num a) => [[a]] -> a

-- zip elements togeather by array position
swapPlaces :: [[a]] -> [[a]]
swapPlaces [] = []
swapPlaces xs = foldr (zipWith (:)) (repeat []) xs 

matMult3 :: (Num a) => [[a]] -> [[a]] -> [[a]]
matMult3 a b = [map (sum . zipWith (*) r) $ swapPlaces b | r <- a]


-- TEST SET FOR Q3
{-
Your functions should have the following behaviour:
trace [[1,2],[6,4]] is Just 5
-}

-- QUESTION 4

{-
triNumber :: Int -> Int -> [Int]
FIRST ARGUMENT IS ROW NUMBER, SECOND IS SEED/VALUE AT TIP OF TRIANGLE
-}

-- TEST SET FOR Q4
{-
Your function should have the following behaviour:
triNumber 3 1 is [2,3,5]
-}

-- QUESTION 5
		   
--Recurisvely assign triple using backwards calculating euclid's algorithm
combine :: Int -> Int -> (Int, Int, Int)
combine 0 b = (0, 1, b)
combine a 0 = (0, 1, a)
combine a b = let (p, o, u) = combine (mod b a) a in (o - (div b a) * p, p, u)
		   
