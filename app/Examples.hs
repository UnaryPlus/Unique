{-# LANGUAGE QuasiQuotes #-}

module Examples (copy) where

import NeatInterpolation (text)

import Data.Text (Text)
import qualified Data.Text.IO as IO

copy :: IO ()
copy = do
  IO.writeFile "truth-machine.uniq" truthMachine
  IO.writeFile "fibonacci.uniq" fibonacci
  IO.writeFile "hello-world.uniq" helloWorld
  putStrLn "Created truth-machine.uniq"
  putStrLn "Created fibonacci.uniq"
  putStrLn "Created hello-world.uniq"

truthMachine :: Text
truthMachine = [text|
  # get input
  42

  # print 1 indefinitely
  0 11 [
    70 -70 + 1 50 49 - 4
    69 -69 + 3 [ 24 20 - 64 60 - 44 ] 34 9
  ] 16 18 +

  # print 0
  68 -68 + 84 80 - [
    67 -67 + 91 90 - 66 -66 + 21 23 +
  ] 15 19 +

  # create if/else statement
  8
  |]

fibonacci :: Text
fibonacci = [text|
  # add 1 and 0 to stack
  0 2 1 70 -70 + 6

  # loop forever
  68 -68 + 55 54 - 57 56 -
  67 -67 + 7 [
    # save a copy of the top number
    84 80 - 3 93 90 -

    # add and print
    10 4 89 85 - 44
  ] 34

  # create while loop
  9
  |]

helloWorld :: Text
helloWorld = [text|
  # add array of characters to stack
  0 14 [
    10
    33     # !
    100    # d
    108    # l
    114    # r
    111    # o
    119    # w
    32
    44     # ,
    37 3 * # o
    27 4 * # l
    12 9 * # l
    101    # e
    104    # h
  ]

  # print characters
  43
  |]
