-- To run install ghc/ghci
-- Run command: ghci 02_turing_machine.hs
-- You can test functions by using testDB and other methods

import qualified Data.List as D
import qualified Data.Maybe as M

{- Bidirectionally infinite tape with an index of one focused element (first
 - element of the second list).
 -
 - Example: a tape where each cell contains its own index, looks like this:
 -
 -     Tape 0 [-1, -2 ..] [0, 1 ..]
 -}
data Tape a = Tape Int [a] [a]

{- Creates a bidirectionally infinite tape filled with a list of initial values
 - (from index 0 onwards) and a default value everywhere else (including all
 - negative indices). Initially focused on index 0.
 -
 - Example:
 -
 -     fromList 42 [1, 2, 3]  ~>*  Tape 0 [42, 42 ..] [1, 2, 3, 42, 42 ..]
 -}
fromList :: a -> [a] -> Tape a
fromList a xs = Tape 0 (repeat a) (xs ++ repeat a) 

{- Returns index of the active (focused) cell. -}
tapeIndex :: Tape a -> Int
tapeIndex (Tape a _ _) = a

{- Returns value of the active cell. -}
readTape :: Tape a -> a
readTape (Tape _ _ (x:_)) = x 

{- Changes value of the active cell. -}
writeTape :: a -> Tape a -> Tape a
writeTape y (Tape a b (_:xs)) = Tape a b (y:xs)

{- Focuses the next cell of the tape. -}
advanceTape :: Tape a -> Tape a
advanceTape (Tape n a (x:xs)) = Tape (n + 1) (x:a) xs

{- Focus the cell on the given index. -}
seekTape :: Int -> Tape a -> Tape a
seekTape num (Tape n (x:xs) (y:ys))
            | num == n = (Tape n (x:xs) (y:ys))
            | num > n  = seekTape num (Tape (n + 1) (y:x:xs) ys)
            | num < n  = seekTape num (Tape (n - 1) xs (x:y:ys))
{-     where 
          back :: Int -> Tape a -> Tape a
          back 0 a = a
          back num (Tape n (a:as) b) = back (num + 1) (Tape (n - 1) as (a:b))
          forward :: Int -> Tape a -> Tape a
          forward 0 a = a
          forward num (Tape n a (b:bs)) = forward (num - 1) (Tape (n + 1) (b:a) bs) -}
 
{- Registers, pseudoregisters and immediate values -}
type Value = Int
data Operand  = RA | RB | RC | RD -- general-purpose registers
              | MI                -- focused memory index
              | M                 -- focused memory (content at index MI)
              | PC                -- program counter
              | Imm Value         -- immediate value (write does nothing)
              deriving Show

{- Internal flags for the conditional instruction -}
data SignumFlag  = ZeroFlag | PosFlag | NegFlag deriving Show
data ParityFlag  = EvenFlag | OddFlag deriving Show
type TestFlags   = (SignumFlag, ParityFlag)

{- Predicates for the conditional instruction -}
data Condition   = Zero | Pos | Neg | Even | Odd deriving Show
{- The instruction set -}
data Instruction = Add    Operand Operand Operand -- r1 := r2 + r3
                 | Halve  Operand Operand         -- r1 := ⌊r2 / 2⌋
                 | Negate Operand Operand         -- r1 := - r2

                 | Test Operand                   -- Fill test flags
                 | If Condition                   -- Skip next instruction unless true

                 | Out Operand                    -- Push a value to the output
                 | Halt                           -- Cease execution
                 deriving Show

{- Machine state -}
type Machine = (Program, Data, Regs, TestFlags)
type Program = Tape Instruction
type Data = Tape Value
type Regs = (Value, Value, Value, Value)

{- Gets a value from the machine based on an operand -}
getValue :: Operand -> Machine -> Value
getValue RA (_, _, (a, _, _, _), _) = a
getValue RB (_, _, (_, b, _, _), _) = b
getValue RC (_, _, (_, _, c, _), _) = c
getValue RD (_, _, (_, _, _, d), _) = d
getValue PC (tape, _, _, _) = tapeIndex tape
getValue MI (_, tape, _, _) = tapeIndex tape
getValue M  (_, tape, _, _) = readTape tape
getValue (Imm a) _ = a

{- Changes the machine state based on an value assigned to an operand -}
setValue :: Operand -> Value -> Machine -> Machine
setValue RA x (a, b, (_, p, q, r), d) = (a, b, (x, p, q, r), d)
setValue RB x (a, b, (o, _, q, r), d) = (a, b, (o, x, q, r), d)
setValue RC x (a, b, (o, p, _, r), d) = (a, b, (o, p, x, r), d)
setValue RD x (a, b, (o, p, q, _), d) = (a, b, (o, p, q, x), d)
setValue PC x (tape, b, c, d) = ((seekTape x tape), b, c, d)
setValue MI x (a, tape, c, d) = (a, (seekTape x tape), c, d)
setValue M  x (a, tape, c, d) = (a, (writeTape x tape), c, d)
setValue (Imm _) _ pc = pc

setFlags :: Operand -> Machine -> Machine
setFlags op (a, b, c, d) =
      let setSflag op pc
            | (getValue op pc) == 0 = ZeroFlag
            | (getValue op pc) > 0  = PosFlag
            | otherwise             = NegFlag
          setPFlag op pc 
            | even (getValue op pc) = EvenFlag
            | otherwise             = OddFlag
      in (a, b, c, (setSflag op (a, b, c, d), setPFlag op (a, b, c, d)))

nextInstruction :: Machine ->  Machine
nextInstruction (ptape, dtape, regs, flags) = (advanceTape ptape, dtape, regs, flags)

evalInstruction :: Instruction -> Machine -> Maybe (Maybe Value, Machine)
evalInstruction (Add PC ry rz) pc = Just (Nothing, setValue PC ((getValue ry pc) + (getValue rz pc)) pc)
evalInstruction (Add rx ry rz) pc = Just (Nothing, nextInstruction (setValue rx ((getValue ry pc) + (getValue rz pc)) pc))
evalInstruction (Halve  PC ry) pc = Just (Nothing, setValue PC (div (getValue ry pc) 2) pc)
evalInstruction (Halve  rx ry) pc = Just (Nothing, nextInstruction (setValue rx (div (getValue ry pc) 2) pc))
evalInstruction (Negate PC ry) pc = Just (Nothing, setValue PC (negate (getValue ry pc)) pc)
evalInstruction (Negate rx ry) pc = Just (Nothing, nextInstruction (setValue rx (negate (getValue ry pc)) pc))
evalInstruction (Test rx)      pc = Just (Nothing, nextInstruction (setFlags rx pc))
evalInstruction (If Zero) (a, b, c, (ZeroFlag, pFlag)) = Just (Nothing, nextInstruction (a, b, c, (ZeroFlag, pFlag)))
evalInstruction (If Zero) (a, b, c, (sFlag, pFlag))    = Just (Nothing, nextInstruction (nextInstruction (a, b, c, (sFlag, pFlag))))
evalInstruction (If Pos)  (a, b, c, (PosFlag, pFlag))  = Just (Nothing, nextInstruction (a, b, c, (PosFlag, pFlag))) 
evalInstruction (If Pos)  (a, b, c, (sFlag, pFlag))    = Just (Nothing, nextInstruction (nextInstruction (a, b, c, (sFlag, pFlag))))
evalInstruction (If Neg)  (a, b, c, (NegFlag, pFlag))  = Just (Nothing, nextInstruction (a, b, c, (NegFlag, pFlag))) 
evalInstruction (If Neg)  (a, b, c, (sFlag, pFlag))    = Just (Nothing, nextInstruction (nextInstruction (a, b, c, (sFlag, pFlag))))
evalInstruction (If Even) (a, b, c, (sFlag, EvenFlag)) = Just (Nothing, nextInstruction (a, b, c, (sFlag, EvenFlag)))
evalInstruction (If Even) (a, b, c, (sFlag, pFlag))    = Just (Nothing, nextInstruction (nextInstruction (a, b, c, (sFlag, pFlag))))
evalInstruction (If Odd)  (a, b, c, (sFlag, OddFlag))  = Just (Nothing, nextInstruction (a, b, c, (sFlag, OddFlag)))
evalInstruction (If Odd)  (a, b, c, (sFlag, pFlag))    = Just (Nothing, nextInstruction (nextInstruction (a, b, c, (sFlag, pFlag))))
evalInstruction (Out rx)       pc = Just (Just (getValue rx pc), nextInstruction pc)
evalInstruction (Halt)         pc = Nothing

{- Performs one instruction returning the new state and a potential output,
 - unless the machine is to halt. -}
evalStep :: Machine -> Maybe (Maybe Value, Machine)
evalStep (ptape, dtape, regs, flags) = evalInstruction (readTape ptape) (ptape, dtape, regs, flags)

{- Takes program and initial memory (arguments), returns results of all Out
 - instructions in order of their execution. The result must be produced lazily:
 - even if the machine does not halt, every performed Out instruction must
 - appear in the output.
 -}
eval :: [Instruction] -> [Value] -> [Value]
eval a b = M.catMaybes (D.unfoldr (evalStep) (fromList Halt a, fromList 0 b, (0, 0, 0, 0), (ZeroFlag, EvenFlag)))

{- Sample program: copy the initial content of the memory tape up to the first
 - zero into the output stream.
 -
 - E.g., 'eval progEcho [42, 66, 0, 99]' ~>* [42, 66]
 -}
progEcho :: [Instruction]
progEcho = [ Test M             -- check contents of the focused memory index
           , If Zero            -- ... and if it is zero,
           , Halt               -- ... halt the execution
           , Out M           -- otherwise, output the value
           , Add MI MI (Imm 1) -- focus the next memory cell
           , Add PC RA RA      -- jump back to the first instruction (RA is 0)
           ]


{- The Show instance for Tape shows only a first few elements of each list. -}
instance Show a => Show (Tape a) where
    show (Tape i b f) = "Tape " ++ showsPrec 10 i " [" ++ showLim 10 b
                                               ++ " [" ++ showLim 10 f
        where showLim :: Show e => Int -> [e] -> String
              showLim _ [] = "]"
              showLim 0 _ = "...]"
              showLim n (x:xs) = show x ++ "," ++ showLim (n - 1) xs
