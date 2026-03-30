module Main
  ( main
  ) where

import Control.Arrow
import Data.Bifunctor (Bifunctor(bimap))
import Data.Char (isAlpha)
import Text.Printf (printf)

matchingCharsCount :: (a -> Bool) -> [[a]] -> Int
matchingCharsCount filterFunc list = sum $ map (length . filter filterFunc) list

both :: (Bifunctor f) => (a -> b) -> f a a -> f b b
both f = bimap f f

calculateVowelPercentage :: String -> Double
calculateVowelPercentage =
  (* 100.0) . uncurry (/) . both fromIntegral
    <$> (countWithFilter (`elem` "aeiouy") &&& countWithFilter isAlpha)
  where
    countWithFilter filterFunc =
      matchingCharsCount filterFunc . filter ((> 3) . length) . words

main :: IO ()
main = getContents >>= \i -> printf "%.2f%%\n" $ calculateVowelPercentage i
