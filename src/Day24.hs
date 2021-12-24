{-# LANGUAGE DeriveFunctor #-}

module Day24 where

import Control.Arrow (Arrow((&&&)))
import Control.Monad.Except
    ( ExceptT
    , MonadError(catchError, throwError)
    , runExceptT
    , when
    )
import Control.Monad.Identity (Identity(runIdentity), IdentityT(runIdentityT))
import Control.Monad.State
    ( State
    , StateT
    , execState
    , execStateT
    , get
    , modify
    , put
    )
import Data.Bifunctor (Bifunctor(first))
import qualified Data.Map as M
import Text.Parsec ((<|>), char, space, string, try)
import Util.Parser (Parser, integerP, parseList)
import Data.List (inits)

insertWithFrom :: Ord k => (a -> a -> a) -> k -> k -> M.Map k a -> M.Map k a
insertWithFrom f d s m = M.insertWith f d (m M.! s) m

eql :: Eq a => a -> a -> Integer
eql a b =
    if a == b
        then 1
        else 0

toDigits :: Integer -> Ram
toDigits 0 = []
toDigits x = toDigits (x `div` 10) ++ [x `mod` 10]

fromDigits :: Ram -> Integer
fromDigits = foldl1 (\acc x -> acc * 10 + x)

data Reg
    = W
    | X
    | Y
    | Z
    deriving (Show, Ord, Eq)

data Arg
    = Imm Integer
    | Reg Reg
    deriving (Show, Ord, Eq)

data Ins a
    = Inp Reg
    | Add Reg a
    | Mul Reg a
    | Div Reg a
    | Mod Reg a
    | Eql Reg a
    deriving (Show, Ord, Eq, Functor)

type Instr = Ins Arg

type ALU = M.Map Reg Integer

type Ram = [Integer]

isValid :: ALU -> Bool
isValid = (== 0) . (M.! Z)

initALU :: ALU
initALU = M.fromList [(W, 0), (X, 0), (Y, 0), (Z, 0)]

minRam :: Integer
minRam = 11111111111111

maxRam :: Integer
maxRam = 99999999999999

immP :: Parser Integer
immP = toInteger <$> integerP

regP :: Parser Reg
regP = W <$ char 'w' <|> X <$ char 'x' <|> Y <$ char 'y' <|> Z <$ char 'z'

argP :: Parser Arg
argP = Imm <$> immP <|> Reg <$> regP

inpP :: Parser Instr
inpP = Inp <$> (string "inp" >> space >> regP)

addP :: Parser Instr
addP = Add <$> (string "add" >> space >> regP) <*> (space >> argP)

mulModP :: Parser Instr
mulModP =
    char 'm' >>
    (Mul <$> (string "ul" >> space >> regP) <*> (space >> argP) <|>
     Mod <$> (string "od" >> space >> regP) <*> (space >> argP))

divP :: Parser Instr
divP = Div <$> (string "div" >> space >> regP) <*> (space >> argP)

eqlP :: Parser Instr
eqlP = Eql <$> (string "eql" >> space >> regP) <*> (space >> argP)

instrP :: Parser Instr
instrP = inpP <|> addP <|> mulModP <|> divP <|> eqlP

type Input = [Instr]

parseContent :: String -> Input
parseContent = parseList instrP . lines

type Eval = StateT (ALU, Ram) (ExceptT String Identity)

evalM :: Instr -> Eval ()
evalM (Inp r) = modify (\(alu, x:xs) -> (M.insert r x alu, xs))
evalM (Add d (Reg s)) = modify (first (insertWithFrom (+) d s))
evalM (Add d (Imm s)) = modify (first (M.insertWith (+) d s))
evalM (Mul d (Reg s)) = modify (first (insertWithFrom (*) d s))
evalM (Mul d (Imm s)) = modify (first (M.insertWith (*) d s))
evalM (Div d (Reg s)) =
    get >>=
    (\(alu, _) ->
         if alu M.! s == 0
             then throwError "Division by Zero"
             else modify (first (insertWithFrom (flip quot) d s)))
evalM (Div d (Imm s)) =
    get >>=
    (\(alu, _) ->
         if s == 0
             then throwError "Division by Zero"
             else modify (first (M.insertWith (flip quot) d s)))
evalM (Mod d (Reg s)) =
    get >>=
    (\(alu, _) ->
         if alu M.! s <= 0 || alu M.! d < 0
             then throwError "Division by Zero"
             else modify (first (insertWithFrom (flip rem) d s)))
evalM (Mod d (Imm s)) =
    get >>=
    (\(alu, _) ->
         if s <= 0 || alu M.! d < 0
             then throwError "Division by Zero"
             else modify (first (M.insertWith (flip rem) d s)))
evalM (Eql d (Reg s)) = modify (first (insertWithFrom eql d s))
evalM (Eql d (Imm s)) = modify (first (M.insertWith eql d s))

evalListM :: [Instr] -> Eval ()
evalListM = mapM_ evalM

checkRam xs ram =
    case h of
        Left _ -> False
        Right (alu, _) -> isValid alu
  where
    f = execStateT (evalListM xs) (initALU, ram)
    g = runExceptT f
    h = runIdentity g

logSearch :: Integer -> Integer -> [Instr] -> ()
logSearch lo hi xs = do
    let mid = (lo + hi) `div` 2
    let ok = checkRam xs (toDigits mid)
    if not ok
       then logSearch (lo+1) mid xs
       else logSearch mid maxRam xs

-- solution xs = fromDigits $ head $ dropWhile (not . bruteForce xs) ramPermutations

solve1 = id -- map (\x -> bruteForce x (head ramPermutations)) . inits

solve2 = const ()

solve :: String -> String
solve = show . (solve1 &&& solve2) . parseContent
