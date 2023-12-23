insertSecond :: Num a => [a] -> a -> [a]
insertSecond [] _ = []
insertSecond (x:y) a = x : a : y

classify :: String -> Int -> Bool
classify "covid" _ = False
classify "hurricane" x = res
    where res = x > 100
classify "murder" y = res
    where res = y >= 1
classify "drowning" y = res
    where res = y >= 1
classify _ y = res
    where res = y >= 6

isNice :: (Int, Int, Int, Int) -> Bool
isNice (a,b,c,d) =
    let tuple1 = a == 42 || b == 42 || a + b == 42
        tuple2 = b == 42 || c == 42 || b + c == 42
        tuple3 = c == 42 || d == 42 || c + d == 42
    in tuple1 && tuple2 && tuple3

mod3 :: Integral a => a -> a
mod3 2 = 2
mod3 1 = 1
mod3 0 = 0
mod3 x = mod3 (x - 3)

div3 :: Integral a => a -> a
div3 0 = 0
div3 1 = 0
div3 2 = 0
div3 x = 1 + div3 (x - 3)

fakt :: Integral a => a -> a
fakt 0 = 1
fakt x = x * fakt(x - 1)

power :: Integral a => a -> a -> a
power 0 _ = 0
power 1 _ = 1
power _ 0 = 1
power x 1 = x
power x n = x * power x (n - 1)

isPower2 :: Integral a => a -> Bool
isPower2 0 = False
isPower2 1 = True
isPower2 x = even x && isPower2 (x `div` 2)

getLast :: [a] -> a
getLast [x] = x
getLast (x:xs) = getLast xs

stripLast :: [a] -> [a]
stripLast [x] = []
stripLast (x:xs) = x : stripLast xs

nth :: Int -> [a] -> a
nth 1 (x:_) = x
nth n (x:xs) = nth (n - 1) xs

append :: [a] -> [a] -> [a]
append [] ys = ys
append (x:xs) ys = x : append xs ys

listSum :: Num n => [n] -> n
listSum [x] = x
listSum (x:xs) = x + listSum xs

digitSum :: Integer -> Integer -> Integer
digitSum _ 0 = 0
digitSum x y = (mod y x) + (digitSum x (div y x))

walkLength :: [Int] -> Int
walkLength [] = 0
walkLength (x:xs) = 1 + walkLength (drop x (x:xs))

filterGiven :: [a] -> [Bool] -> [a]
filterGiven [] [] = []
filterGiven [] _ = []
filterGiven _ [] = []
filterGiven (x:xs) (b:bs) = 
    if b 
    then x : filterGiven xs bs
    else filterGiven xs bs

sumProcess :: Num a => [a] -> [a]
sumProcess [] = []
sumProcess [a] = [a]
sumProcess (x:xs) = x : sumProcess (f x xs)
    where f x (a:as) = (a + x) : as

removeMaybe :: [Maybe a] -> [a]
removeMaybe x = [ a | Just a <- x]

joinWith :: (a -> a -> a) -> [[a]] -> [a]
joinWith _ [] = []
joinWith _ [x] = x
joinWith op (x:y:r) = x ++  [(f op x y)] ++ joinWith op (y:r)
    where f op a b = op (last a) (head b)

multiplesOf :: Integral a => a -> [a]
multiplesOf n = f n
    where f a = a : f (a + n)

makeOdd :: Integer -> Integer
makeOdd x =
    if mod x 2 == 1 then x
    else x + 1

data Sign = Zero | Pos | Neg deriving Show
foo :: Int -> Sign
foo a 
    | a == 0    = Zero
    | a > 0     = Pos
    | otherwise = Neg


data Empire = Rules [Colony]
            deriving Show

data Colony = Plain
            | Settlement Integer
            | Districts [Colony]
            deriving Show

totalPeople :: Empire -> Integer
totalPeople (Rules []) = 0
totalPeople (Rules a) = count a
    where count [] = 0
          count ((Plain):xs) = count xs
          count ((Settlement a):xs) = count xs + a
          count ((Districts a):xs) = count a + count xs



fn :: [Int] -> [Int]
fn lst = [sum (drop count lst) | count <- [1,2 .. length lst]]

fn' :: [Int] -> [Int]
fn' [] = []
fn' (x:xs) = cnt xs : fn' xs
    where cnt [] = 0
          cnt (x:xs) = x + cnt xs

data TreTree a = Node (a, a) (TreTree a, TreTree a, TreTree a)
               | Leaf a

monoLeaf :: Int -> TreTree Int -> Bool
monoLeaf num (Leaf a) = a == num
monoLeaf num (Node _ (a, b, c)) = monoLeaf num a && monoLeaf num b && monoLeaf num c

f1 = getLine >>= putStrLn
f2 = getLine >>= putStrLn . reverse
f3 = getLine >>= \s -> putStrLn (if s == "" then "<empty>" else s)

suma :: [Int] -> Int
suma [] = 0
suma (x:xs) = x + suma xs

qSort :: Ord a => [a] -> [a]
qSort [] = []
qSort (p:s) = qSort [ x | x<-s, x<p ]
              ++ [p] ++
              qSort [ x | x<-s, x>=p ]

loopecho :: IO ()
loopecho = getLine >>= \s -> if null s then return () else putStrLn s >> loopecho

query :: String -> IO Bool
query question = do putStrLn question
                    answer <- getLine
                    pure (answer == "ano")

queryB :: String -> IO Bool
queryB question = putStrLn question >> getLine >>= \a -> if null a then queryB question else pure (a == "ano" || a == "Ano")


decodeRLE :: [(Int, Char)] -> IO String
decodeRLE a = pure (foldl (++) ("") (map makeStr a))
    where makeStr (num, ch) = replicate num ch

decodeRLE2 :: [(Int, Char)] -> IO String
decodeRLE2 a = pure ( concat [ x | (num, ch) <- a, let x = replicate num ch])

multipy :: Int -> [a] -> [a]
multipy n xs = concat (map (\x -> replicate n x) xs)

rotateL :: Int -> [a] -> [a]
rotateL n ls = drop (abs n) ls ++ take (abs n) ls

test = [ s ++ t  | a <- [1..6], let b = 6 - a, let s = concat (replicate b "b"), let t = concat (replicate (a - 1) "a") ]

safeDiv :: Integral a => a -> a -> Maybe a
safeDiv _ 0 = Nothing
safeDiv x y = Just (x `div` y)

negaticeCredit :: [(String, Integer)] -> [String]
negaticeCredit l = [n | (n, k) <- l, k >= 0]

f :: [Char] -> Int -> [Char]
f ls n = concat (map (replicate n) ls)

what :: [Int] -> [Bool] -> [(Bool, Int)]
what a b = zip b a

applyEither :: (a -> b -> c) -> Either e a -> Either e b -> Either e c
applyEither = undefined

data Foo a b c = A a b
               | B b c

foo :: (a -> b -> c) -> (b -> c -> d) -> (d -> e) -> Foo a b c -> e
foo opABC opBCD opDE (A a b) = opDE (opBCD b (opABC a b))
foo opABC opBCD opDE (B b c) = opDE (opBCD b c)

data RoseTree a = RoseNode a [RoseTree a]

stripedSum :: Num n => RoseTree n -> Bool -> n
stripedSum (RoseNode num []) boo = if boo then num else 0
stripedSum (RoseNode num trees) boo = if boo
                                  then num + sum (map (flip stripedSum (not boo)) trees)
                                  else sum (map (flip stripedSum (not boo)) trees)

splitToAscending :: Ord a => [a] -> [[a]]
splitToAscending xs = foldr append [] xs

append :: Ord a => a -> [[a]]-> [[a]]
append a ((p:px):xs)
    | a < p    = (a:p:px) : xs
    | otherwise = [a] : (p:px) : xs
append a _ = [[a]]

data ChristmasTree = Branch [ChristmasTree]
                   | Trinket
                   | LightChain Int -- number of lights

ctFold :: ([a] -> a) -> a -> (Int -> a) -> ChristmasTree -> a
ctFold b t l (Branch bs)    = b (map (ctFold b t l) bs)
ctFold _ t _ Trinket        = t
ctFold _ _ l (LightChain n) = l n

powerUse :: Int -> ChristmasTree -> Int
powerUse pow ct = ctFold sum 0 (* pow) ct

filterOutSame :: Eq a => [(a, a)] -> [(a, a)]
filterOutSame x = filter (\(x,y) -> x == y) x

foon :: Integral a => a -> a
foon = (`mod` 7) . (^ 2)

listGCD :: Integral a => [a] -> Maybe a
listGCD [] = Nothing
listGCD [a] = Just a
listGCD (a:b:xs) = listGCD ((gcd a b) : xs)

data BinTree a = Empty
               | Node a (BinTree a) (BinTree a)

sumFromLevel :: Num a => BinTree a -> Int -> a
sumFromLevel tree num = foo tree 0 num
    where foo Empty _ _ = 0
          foo (Node n a b) now lvl = if lvl <= now
                                     then n + foo a (now + 1) lvl + foo b (now + 1) lvl
                                     else foo a (now + 1) lvl + foo b (now + 1) lvl

isEmpty :: [a] -> Bool
isEmpty [] = False
isEmpty [_] = True

myHead :: [a] -> a
myHead (x:y) = x

myTail :: [a] -> [a]
myTail (x:y) = y

maxPairs :: Ord a => [(a, a)] -> [a]
maxPairs [] = []
maxPairs x = map f x 
    where f (a, b) = if a > b then a else b

filterOutShorter :: [String] -> Int -> [String]
filterOutShorter xs n = filter (\ s -> length s >= n) xs

getNames :: [(String, Integer)] -> [String]
getNames xs = map fst xs

successfulRecords :: [(String, Integer)] -> [(String, Integer)]
successfulRecords xs = filter (\ (_, y) -> y >= 8) xs

successfulNames :: [(String, Integer)] -> [String]
successfulNames xs = getNames (successfulRecords xs)

successfulStrings :: [(String, Integer)] -> [String]
successfulStrings x = map f (successfulRecords x)
    where f :: (String, Integer) -> String
          f (n, num) = n ++ ": " ++ show num ++ "b"

myZip :: [a] -> [b] -> [(a, b)]
myZip xs ys = zipWith (,) xs ys

assignPrizes :: [String] -> [Integer] -> [(String, Integer)]
assignPrizes = zip

prizeTexts :: [String] -> [Integer] -> [String]
prizeTexts xs ys = zipWith f xs ys
    where f a b = a ++ ": " ++ show b ++ "kc"

countStudentsByPoints :: Integer -> [(String, Integer)] -> Int
countStudentsByPoints pt s = length (filter ((pt==) . snd) s)

studentNamesByPoints :: Integer -> [(String, Integer)] -> [String]
studentNamesByPoints pt s = getNames (filter ((== pt) . snd) s)

merge :: Either e (Either e a) -> Either e a
merge (Left a) = Left a
merge (Right(Left a)) = Left a
merge (Right(Right a)) = Right a

evenOddMap :: (a -> b) -> (a -> b) -> [a] -> [b]
evenOddMap a b x = foo a b x 0
    where foo a b [] index = []
          foo a b (x: xs) index = if odd index 
                                  then (b x) : foo a b xs (index + 1)
                                  else (a x) : foo a b xs (index + 1)