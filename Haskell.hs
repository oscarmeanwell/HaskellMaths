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

-- zip elements togeather by array position
swapPlaces :: [[a]] -> [[a]]
swapPlaces [] = []
swapPlaces xs = foldr (zipWith (:)) (repeat []) xs 

matMult3 :: (Num a) => [[a]] -> [[a]] -> [[a]]
matMult3 a b = [map (sum . zipWith (*) r) $ swapPlaces b | r <- a]

triNumber :: Int -> Int -> [Int]

combine :: Int -> Int -> (Int, Int, Int)
combine 0 b = (0, 1, b)
combine a 0 = (0, 1, a)
combine a b = let (p, o, u) = combine (mod b a) a in (o - (div b a) * p, p, u)
		   
