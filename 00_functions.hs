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
