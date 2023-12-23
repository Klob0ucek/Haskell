-- To run install ghc/ghci
-- Run command: ghci 01_public_transport.hs
-- You can test functions by using testDB and other methods

import qualified Data.List as D
import qualified Data.Maybe as M

data OpType = Add | Mul deriving Show

data Expr a = Con a
            | Var String
            | Op OpType [Expr a]
            | Let String (Expr a) (Expr a)
            deriving Show

isVar :: Expr a -> Bool
isVar (Var _) = True
isVar _ = False

isCon :: Expr a -> Bool
isCon (Con _) = True
isCon _ = False

isLet :: Expr a -> Bool
isLet (Let _ _ _) = True
isLet _ = False

isOp :: Expr a -> Bool
isOp (Op _ _ ) = True
isOp _ = False

freeVars :: Expr a -> [String]
freeVars (Var x) = [x]
freeVars (Con _) = []
freeVars (Op _ []) = []
freeVars (Op o (x:xs)) = D.nub $ freeVars x ++ freeVars (Op o xs)
freeVars (Let x (Var a) b) = D.nub $ (a) : freeVars b
freeVars (Let x a b) = D.nub $ (freeVars a) ++ (remove x (freeVars b))
      where remove :: String -> [String] -> [String]
            remove _ [] = []
            remove x (a:b) = if x == a
                             then remove x b
                             else a : remove x b
-- freeVars (Let "w" (Op Mul [Var "w"]) (Con 0)) -- ["w"]

simplifyArity01 :: Num a => Expr a -> Expr a
simplifyArity01 (Con x) = Con x
simplifyArity01 (Var x) = Var x
simplifyArity01 (Op Mul []) = Con 1
simplifyArity01 (Op Add []) = Con 0
simplifyArity01 (Op _ [(Con x)]) = Con x 
simplifyArity01 (Op _ [x]) = simplifyArity01 x
simplifyArity01 (Op o x) = Op o (simplifyList x)
    where simplifyList :: Num a => [Expr a] -> [Expr a]
          simplifyList [] = []
          simplifyList (x:xs) = simplifyArity01 x : simplifyList xs
simplifyArity01 (Let a b c) = Let a (simplifyArity01 b) (simplifyArity01 c)

evalAll :: Num a => [Expr a] -> [(String, a)] -> [Maybe a]
evalAll [] _ = []
evalAll ((Con x):xs) key = (Just x) : evalAll xs key
evalAll ((Var x):xs) key =  (lookup x key) : evalAll xs key
evalAll ((Let x a b):xs) key = (eval (Let x a b) key) : evalAll xs key
evalAll ((Op Add x): xs) key = (eval (Op Add x) key) : evalAll xs key
evalAll ((Op Mul x): xs) key = (eval (Op Mul x) key) : evalAll xs key

nothingIn :: Num a => [Maybe a] -> [Bool]
nothingIn [] = []
nothingIn (x:xs) = M.isJust x : nothingIn xs

varInB :: String -> [String] -> Bool
varInB _ [] = False
varInB a (s:xs) = if a == s
                  then True
                  else varInB a xs

addLetToKey :: Num a => Expr a -> [(String, a)] -> [(String, a)]
addLetToKey (Let x a b) key = (x, M.fromJust (eval a key)) : key

eval :: Num a => Expr a -> [(String, a)] -> Maybe a
eval (Con x) _ = Just x
eval (Var x) key = lookup x key
eval (Let a (Con x) (Con y)) _ = Just y
eval (Let x (Con a) (Var b)) _ = if x == b
                                 then Just a
                                 else Nothing
eval (Let x (Var a) b) key = if varInB a (freeVars b)
                             then eval b key
                             else Nothing                
eval (Let x a b) key = if varInB x (freeVars b)
                       then eval b (addLetToKey (Let x a b) key)
                       else eval b key
eval (Op o []) key = Just (getConValue $ simplifyArity01 (Op o []))
    where getConValue :: Num a => Expr a -> a
          getConValue (Con x) = x
eval (Op Add x) key = if and (nothingIn (evalAll x key))
                      then Just (foldr1 (+) (M.catMaybes (evalAll x key)))
                      else Nothing
eval (Op Mul x) key = if and (nothingIn (evalAll x key))
                        then Just (foldr1 (*) (M.catMaybes (evalAll x key)))
                      else Nothing
-- eval (Let "x" (Con 7) (Let "x" (Con 42) (Var "x"))) [("x", 100)] -- Just 42
-- eval (Let "x" (Var "rs") (Con 0)) [] -- Nothing

flatMul :: [Expr a] -> [Expr a]
flatMul [] = []
flatMul ((Op Mul x):xs) = (flatMul x) ++ flatMul xs
flatMul ((Op Add x):xs) = (flatten (Op Add x)) : flatMul xs
flatMul (x:xs) = flatten x : flatMul xs

flatAdd :: [Expr a] -> [Expr a]
flatAdd [] = []
flatAdd ((Op Add x):xs) = (flatAdd x) ++ flatAdd xs
flatAdd ((Op Mul x):xs) = (flatten (Op Mul x)) : flatAdd xs
flatAdd (x:xs) = flatten x : flatAdd xs

flatten :: Expr a -> Expr a
flatten (Con x) = Con x
flatten (Var x) = Var x
flatten (Op Mul x) = Op Mul (flatMul x)
flatten (Op Add x) = Op Add (flatAdd x)
flatten (Let x a b) = Let x (flatten a) (flatten b)

simplifyIdentity :: (Eq a, Num a) => Expr a -> Expr a
simplifyIdentity (Var x) = Var x
simplifyIdentity (Con x) = Con x
simplifyIdentity (Op Add x) = Op Add (simplifyIdListAdd x)
      where simplifyIdListAdd :: (Eq a, Num a) => [Expr a] -> [Expr a]
            simplifyIdListAdd [] = []
            simplifyIdListAdd ((Con 0):xs) = simplifyIdListAdd xs
            simplifyIdListAdd ((Op Add x):xs) = (Op Add (simplifyIdListAdd x)) : simplifyIdListAdd xs
            simplifyIdListAdd ((x):xs) = simplifyIdentity x : simplifyIdListAdd xs

simplifyIdentity (Op Mul x) = Op Mul (simplifyIdListMul x) 
      where simplifyIdListMul :: (Eq a, Num a) => [Expr a] -> [Expr a]
            simplifyIdListMul [] = []
            simplifyIdListMul ((Con 1):xs) = simplifyIdListMul xs
            simplifyIdListMul ((Op Mul x):xs) = (Op Mul (simplifyIdListMul x)) : simplifyIdListMul xs
            simplifyIdListMul ((x):xs) = simplifyIdentity x : simplifyIdListMul xs
simplifyIdentity (Let x a b) = Let x (simplifyIdentity a) (simplifyIdentity b)

annihiList :: (Eq a, Num a) => [Expr a] -> [Expr a]
annihiList [] = []
annihiList ((Var x):xs) = (Var x) : annihiList xs
annihiList ((Con x):xs) = (Con x) : annihiList xs
annihiList ((Op Mul x):xs) = simplifyAnnihilating (Op Mul x) : annihiList xs
annihiList ((Op Add x):xs) = (Op Add (annihiList x)) : annihiList xs
annihiList ((Let x a b):xs) = (Let x (simplifyAnnihilating a) (simplifyAnnihilating b)) : annihiList xs

containConZero :: (Eq a, Num a) => [Expr a] -> Bool
containConZero [] = False
containConZero ((Con x):xs) = if x == 0 then True else containConZero xs
containConZero (x:xs) = containConZero xs

simplifyAnnihilating :: (Eq a, Num a) => Expr a -> Expr a
simplifyAnnihilating (Var x) = Var x
simplifyAnnihilating (Con x) = Con x
simplifyAnnihilating (Op Mul x) = if containConZero (annihiList x)
                                  then (Con 0)
                                  else Op Mul (annihiList x)
simplifyAnnihilating (Op Add x) = Op Add (annihiList x)
simplifyAnnihilating (Let x a b) = Let x (simplifyAnnihilating a) (simplifyAnnihilating b)

fToInt :: Num a => Expr a -> [a]
fToInt (Op _ []) = []
fToInt (Op  o ((Con x):xs)) = x : fToInt (Op o xs)

simplifyConstants :: (Eq a, Num a) => Expr a -> Expr a
simplifyConstants (Con x) = Con x
simplifyConstants (Var x) = Var x
simplifyConstants (Op Mul x) = simplifyIdentity $ simplifyAnnihilating $ flatten $ simplifyArity01 (Op Mul x)
simplifyConstants (Op Add x) = if freeVars (Op Add x) /= []
                               then simplifyIdentity $ simplifyAnnihilating $ flatten $ simplifyArity01 (Op Add x)
                               else calcAdd $ fToInt $ simplifyIdentity $ simplifyAnnihilating $ flatten $ simplifyArity01 (Op Add x)
      where calcAdd :: Num a => [a] -> Expr a
            calcAdd x = Con (foldr1 (+) x)
