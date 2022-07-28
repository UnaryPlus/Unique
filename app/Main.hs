{-
  (c) 2022 Owen Bechtel
  License: MIT (see LICENSE file)
-}

{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Control.Monad.Trans.State (StateT, runStateT, gets, modify)

import Text.Read (readMaybe)
import Data.List (nub)
import Data.Char (ord, chr)
import Data.Function (on)
import Data.Bifunctor (first, second)
import Control.Applicative (liftA2)
import Control.Monad (foldM, void, when)
import Control.Exception (IOException, catch)
import System.Environment (getArgs)

main :: IO ()
main = getArgs >>= \case
  [] -> putStrLn "ERROR: you must supply a file to run"
  [path] -> runFile path
  _ -> putStrLn "ERROR: too many arguments"

runFile :: FilePath -> IO ()
runFile path = safeReadFile path >>= \case
  Nothing -> putStrLn ("ERROR: file '" ++ path ++ "' does not exist")
  Just program -> do
    runUnique (runProgram program) >>= \case
      Left err -> putStrLn ("ERROR: " ++ err)
      Right () -> return ()

safeReadFile :: FilePath -> IO (Maybe String)
safeReadFile path =
  fmap Just (readFile path) `catch`
  \e -> return (const Nothing (e :: IOException))

type Stack = [[Int]]

insert :: Int -> Stack -> Stack
insert num = \case
  [] -> [[num]]
  ([] : stack) -> [num] : stack
  (xs : stack) -> (num : xs) : stack

data Mode = CommandMode | LengthMode | PushMode Int

type Unique = ExceptT String (StateT (Stack, Mode) IO)

runUnique :: Unique a -> IO (Either String a)
runUnique u = fmap fst
  (runStateT (runExceptT u) ([], CommandMode))

fromIO :: IO a -> Unique a
fromIO = lift . lift

getStack :: Unique Stack
getStack = lift (gets fst)

putStack :: Stack -> Unique ()
putStack = modifyStack . const

modifyStack :: (Stack -> Stack) -> Unique ()
modifyStack = lift . modify . first

getMode :: Unique Mode
getMode = lift (gets snd)

putMode :: Mode -> Unique ()
putMode = modifyMode . const

modifyMode :: (Mode -> Mode) -> Unique ()
modifyMode = lift . modify . second

runProgram :: String -> Unique ()
runProgram program = do
  case evalProgram (removeComments program) of
    Left err -> throwE err
    Right commands -> execCommands commands

removeComments :: String -> String
removeComments =
  unlines . map removeComment . lines
  where removeComment = takeWhile (/= '#')

execCommands :: [Int] -> Unique ()
execCommands commands = do
  mapM_ execute commands
  mode <- getMode
  case mode of
    CommandMode -> return ()
    _ -> throwE "fewer numbers in array than expected"

execute :: Int -> Unique ()
execute num = getMode >>= \case
  CommandMode -> execCommand num
  LengthMode ->
    if num <= 0
      then throwE "array length must be positive"
      else putMode (PushMode num)
  PushMode len -> do
    modifyStack (insert num)
    putMode (if len == 1
      then CommandMode
      else PushMode (len - 1))

execCommand :: Int -> Unique ()
execCommand = \case
  --PUSH
  0 -> do
    push []
    putMode LengthMode

  --DELETE
  1 -> void pop

  --SWAP
  2 -> do
    xs <- pop
    ys <- pop
    push xs
    push ys

  --ROTATE
  3 -> do
    xs <- pop
    ys <- pop
    zs <- pop
    push ys
    push xs
    push zs

  --DUPLICATE
  4 -> do
    xs <- pop
    push xs
    push xs

  --APPEND
  5 -> do
    xs <- pop
    ys <- pop
    push (xs ++ ys)

  --UNFOLD
  6 -> do
    xs <- pop
    mapM_ (push . return) (reverse xs)

  --IF
  7 -> do
    commands <- pop
    conditions <- pop
    when (all truthy conditions)
      (execCommands commands)

  --IF ELSE
  8 -> do
    elseCommands <- pop
    commands <- pop
    conditions <- pop
    if all truthy conditions
      then execCommands commands
      else execCommands elseCommands

  --WHILE
  9 -> do
    commands <- pop
    whileLoop commands

  --ADD
  10 -> apOp (+)
  11 -> zipOp (+)

  --SUBTRACT
  12 -> apOp (-)
  13 -> zipOp (-)

  --MULTIPLY
  14 -> apOp (*)
  15 -> zipOp (*)

  --DIVIDE
  16 -> apOpM divide
  17 -> zipOpM divide

  --MODULO
  18 -> apOpM modulo
  19 -> zipOpM modulo

  --POWER
  20 -> apOpM power
  21 -> zipOpM power

  --OR
  22 -> boolApOp ((||) `on` truthy)
  23 -> boolZipOp ((||) `on` truthy)

  --AND
  24 -> boolApOp ((&&) `on` truthy)
  25 -> boolZipOp ((&&) `on` truthy)

  --LESS THAN
  26 -> boolApOp (<)
  27 -> boolZipOp (<)

  --GREATER THAN
  28 -> boolApOp (>)
  29 -> boolZipOp (>)

  --EQUAL TO
  30 -> boolApOp (==)
  31 -> boolZipOp (==)

  --NEGATE
  32 -> do
    xs <- pop
    push (map negate xs)

  --NOT
  33 -> do
    xs <- pop
    push (map (toInt . not . truthy) xs)

  --REVERSE
  34 -> do
    xs <- pop
    push (reverse xs)

  --LENGTH
  35 -> foldOp length

  --SUM
  36 -> foldOp sum

  --PRODUCT
  37 -> foldOp product

  --ANY
  38 -> foldOp (toInt . any truthy)

  --ALL
  39 -> foldOp (toInt . all truthy)

  --INPUT CHARACTER
  40 -> do
    ch <- fromIO getChar
    push [ord ch]

  --INPUT STRING
  41 -> do
    str <- fromIO getLine
    push (map ord str)

  --INPUT NUMBER
  42 -> do
    num <- fromIO getLine
    case readMaybe num of
      Nothing -> throwE "input is not a number"
      Just x -> push [x]

  --OUTPUT CHARACTERS
  43 -> do
    xs <- pop
    fromIO (putStr (map chr xs))

  --OUTPUT NUMBERS
  44 -> do
    xs <- pop
    fromIO (mapM_ print xs)

  --OTHER
  num -> throwE
    ("no command associated with number '" ++ show num ++ "'")

truthy :: Int -> Bool
truthy = (/= 0)

toInt :: Bool -> Int
toInt False = 0
toInt True = 1

pop :: Unique [Int]
pop = getStack >>= \case
  [] -> throwE "no values in stack to pop"
  (xs : new) -> do
    putStack new
    return xs

push :: [Int] -> Unique ()
push xs = modifyStack (xs:)

apOp :: (Int -> Int -> Int) -> Unique ()
apOp f = do
  xs <- pop
  ys <- pop
  push (liftA2 f ys xs)

zipOp :: (Int -> Int -> Int) -> Unique ()
zipOp f = do
  xs <- pop
  ys <- pop
  push (zipWith f ys xs)

apOpM :: (Int -> Int -> Unique Int) -> Unique ()
apOpM f = do
  xs <- pop
  ys <- pop
  res <- sequence (liftA2 f ys xs)
  push res

zipOpM :: (Int -> Int -> Unique Int) -> Unique ()
zipOpM f = do
  xs <- pop
  ys <- pop
  res <- sequence (zipWith f ys xs)
  push res

boolApOp :: (Int -> Int -> Bool) -> Unique ()
boolApOp f = apOp (\x y -> toInt (f x y))

boolZipOp :: (Int -> Int -> Bool) -> Unique ()
boolZipOp f = zipOp (\x y -> toInt (f x y))

foldOp :: ([Int] -> Int) -> Unique ()
foldOp f = do
  xs <- pop
  push [f xs]

divide :: Int -> Int -> Unique Int
divide x y =
  if y == 0
    then throwE "division by zero"
    else return (div x y)

modulo :: Int -> Int -> Unique Int
modulo x y =
  if y == 0
    then throwE "modulo by zero"
    else return (mod x y)

power :: Int -> Int -> Unique Int
power x y =
  if y < 0
    then throwE "negative exponent"
    else return (x ^ y)

whileLoop :: [Int] -> Unique ()
whileLoop commands = do
  conditions <- pop
  when (all truthy conditions) $ do
    execCommands commands
    whileLoop commands

data Symbol = Lit Int | Add | Subtract | Multiply | Ignore

evalProgram :: String -> Either String [Int]
evalProgram program = do
  symbols <- mapM readSymbol (words program)
  if getNums symbols == nub (getNums symbols)
  then reverse <$> foldM execSymbol [] symbols
    else Left "number occurs more than once"

getNums :: [Symbol] -> [Int]
getNums = \case
  [] -> []
  (Lit x : ss) -> x : getNums ss
  (_ : ss) -> getNums ss

readSymbol :: String -> Either String Symbol
readSymbol = \case
  "+" -> return Add
  "-" -> return Subtract
  "*" -> return Multiply
  "[" -> return Ignore
  "]" -> return Ignore
  sym -> case readMaybe sym of
    Nothing -> Left ("cannot parse symbol '" ++ sym ++ "'")
    Just x -> return (Lit x)

execSymbol :: [Int] -> Symbol -> Either String [Int]
execSymbol xs = \case
  Lit x -> return (x : xs)
  Add -> operation (+) "add"
  Subtract -> operation (-) "subtract"
  Multiply -> operation (*) "multiply"
  Ignore -> return xs
  where
    operation f name =
      case xs of
        (x : y : t) -> return (f y x : t)
        _ -> Left ("not enough numbers to " ++ name)
