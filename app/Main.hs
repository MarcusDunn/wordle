module Main where

{-# LANGUAGE OVERLOADEDSTRINGS #-#-}

import Data.Char (isAlpha, toLower)
import Data.Maybe (catMaybes)
import Data.String (IsString (..))
import Lib

data WordleWord = WordleWord Char Char Char Char Char

data CharResponse = X | G | Y deriving (Eq)

data Response = Response CharResponse CharResponse CharResponse CharResponse CharResponse deriving (Eq)

instance Show WordleWord where
  show (WordleWord a b c d e) = [a, b, c, d, e]

getRule :: WordleRule -> WordleWord -> Bool
getRule (WordleRule f) = f

instance IsString WordleWord where
  fromString word = case fromStringMaybe word of
    Just w -> w
    Nothing -> error "invalid wordleWord"

instance Show Response where
  show (Response a b c d e) = charRespToChar <$> [a, b, c, d, e]
    where
      charRespToChar X = 'X'
      charRespToChar G = 'G'
      charRespToChar Y = 'Y'

newtype WordleRule = WordleRule (WordleWord -> Bool)

instance Semigroup WordleRule where
  (WordleRule f) <> (WordleRule g) = WordleRule (\w -> f w && g w)

instance Monoid WordleRule where
  mempty = WordleRule (const True)

charIsHere :: Char -> Int -> WordleRule
charIsHere g 1 = WordleRule (\(WordleWord a _ _ _ _) -> a == g)
charIsHere g 2 = WordleRule (\(WordleWord _ b _ _ _) -> b == g)
charIsHere g 3 = WordleRule (\(WordleWord _ _ c _ _) -> c == g)
charIsHere g 4 = WordleRule (\(WordleWord _ _ _ d _) -> d == g)
charIsHere g 5 = WordleRule (\(WordleWord _ _ _ _ e) -> e == g)

charIsNotHere :: Char -> Int -> WordleRule
charIsNotHere g 1 = WordleRule (\(WordleWord a _ _ _ _) -> a /= g)
charIsNotHere g 2 = WordleRule (\(WordleWord _ b _ _ _) -> b /= g)
charIsNotHere g 3 = WordleRule (\(WordleWord _ _ c _ _) -> c /= g)
charIsNotHere g 4 = WordleRule (\(WordleWord _ _ _ d _) -> d /= g)
charIsNotHere g 5 = WordleRule (\(WordleWord _ _ _ _ e) -> e /= g)

charIsSomewhere :: Char -> WordleRule
charIsSomewhere c = WordleRule (\(WordleWord c1 c2 c3 c4 c5) -> c `elem` [c1, c2, c3, c4, c5])

charIsNotAnywhere :: Char -> WordleRule
charIsNotAnywhere c = WordleRule (\(WordleWord w1 w2 w3 w4 w5) -> c `notElem` [w1, w2, w3, w4, w5])

respToWordleRule :: WordleWord -> Response -> WordleRule
respToWordleRule (WordleWord w1 w2 w3 w4 w5) (Response r1 r2 r3 r4 r5) = go [(1, w1, r1), (2, w2, r2), (3, w3, r3), (4, w4, r4), (5, w5, r5)]
  where
    go :: [(Int, Char, CharResponse)] -> WordleRule
    go [] = mempty
    go ((i, c, r) : xs) =
      ( case r of
          X -> charIsNotAnywhere c
          G -> charIsHere c i
          Y -> charIsNotHere c i <> charIsSomewhere c
      )
        <> go xs

play :: [WordleWord] -> WordleWord -> IO Int
play xs word = go mempty xs word
  where
    go :: WordleRule -> [WordleWord] -> WordleWord -> IO Int
    go _ [word] _ = do
      print $ "found " ++ show word
      return 0
    go _ [] _ = error "something weird happended"
    go rule xs word = do
      let nextGuess = chooseGuess xs
      let resp = guess word nextGuess
      if resp == Response G G G G G
        then go rule [nextGuess] word
        else do
          let newRule = rule <> respToWordleRule nextGuess resp
          let possible = filter (getRule newRule) xs
          count <- go newRule possible word
          return (count + 1)

chooseGuess :: [WordleWord] -> WordleWord
chooseGuess = byVowels

byVowels :: [WordleWord] -> WordleWord
byVowels (x : xs) = foldr (\acc x -> if vowelCount acc < vowelCount x then x else acc) x xs
  where
    vowelCount :: WordleWord -> Int
    vowelCount (WordleWord w1 w2 w3 w4 w5) = length $ filter isVowel (unique [w1, w2, w3, w4, w5])
    isVowel :: Char -> Bool
    isVowel 'a' = True
    isVowel 'e' = True
    isVowel 'i' = True
    isVowel 'o' = True
    isVowel 'u' = True
    isVowel 'y' = True
    isVowel _ = False

unique :: Eq a => [a] -> [a]
unique = foldr (\x acc -> if x `elem` acc then acc else acc ++ [x]) []

fromStringMaybe :: String -> Maybe WordleWord
fromStringMaybe [a, b, c, d, e]
  | isAlpha a
      && isAlpha b
      && isAlpha c
      && isAlpha d
      && isAlpha e
      && toLower a `elem` ['a' .. 'z']
      && toLower b `elem` ['a' .. 'z']
      && toLower c `elem` ['a' .. 'z']
      && toLower d `elem` ['a' .. 'z']
      && toLower e `elem` ['a' .. 'z'] =
    Just $ WordleWord (toLower a) (toLower b) (toLower c) (toLower d) (toLower e)
fromStringMaybe _ = Nothing

guess :: WordleWord -> WordleWord -> Response
guess (WordleWord a1 a2 a3 a4 a5) (WordleWord g1 g2 g3 g4 g5) =
  Response
    (indivGuess a1 g1 [a2, a3, a4, a5])
    (indivGuess a2 g2 [a1, a3, a4, a5])
    (indivGuess a3 g3 [a1, a2, a4, a5])
    (indivGuess a4 g4 [a1, a2, a3, a5])
    (indivGuess a5 g5 [a1, a2, a3, a4])
  where
    indivGuess :: Char -> Char -> String -> CharResponse
    indivGuess a g rst
      | a == g = G
      | g `elem` rst = Y
      | otherwise = X

main :: IO ()
main = do
  p <- dictWords
  ints <- mapM (play (catMaybes p)) (catMaybes p)
  print $ toRational (sum ints) / toRational (length ints)

dictWords :: IO [Maybe WordleWord]
dictWords = do
  contents <- readFile "/usr/share/dict/words"
  return $ fromStringMaybe <$> words contents