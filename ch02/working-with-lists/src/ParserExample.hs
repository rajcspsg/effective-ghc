module ParserExample where

import Data.List (splitAt)

takeCharacters :: Int -> String -> (String, String)
takeCharacters numCharacters inputString =
  splitAt numCharacters inputString

takeCharacters2 :: Int -> String -> (String, String)
takeCharacters2 numCharacters = stringParser
  where
    stringParser :: String -> (String, String)
    stringParser = \inputString ->
      splitAt numCharacters inputString

data StringParser = StringParser {runStringParser :: String -> (String, String)}

takeCharacters3 :: Int -> StringParser
takeCharacters3 numCharacters = StringParser stringParser
  where
    stringParser :: String -> (String, String)
    stringParser = \inputString ->
      splitAt numCharacters inputString

takeCharacters4 :: Int -> StringParser
takeCharacters4 numCharacters = StringParser $ \inputString ->
  splitAt numCharacters inputString

getNextWord :: StringParser
getNextWord = StringParser $ \someString ->
  case break (== ' ') someString of
    (nextWord, "") -> (nextWord, "")
    (nextWord, rest) -> (nextWord, drop 1 rest)

combineParsers :: StringParser -> StringParser -> StringParser
combineParsers firstParser secondParser = StringParser $ \someString ->
  let (_firstPart, firstResult) = runStringParser firstParser someString
   in runStringParser secondParser firstResult

getNextWordAfterTenLetters :: StringParser
getNextWordAfterTenLetters = combineParsers (takeCharacters4 10) getNextWord

tenLettersAfterFirstWord :: StringParser
tenLettersAfterFirstWord =
  combineParsers getNextWord (takeCharacters4 10)

parseString :: StringParser -> String -> String
parseString parser inputString =
  fst $ runStringParser parser inputString
