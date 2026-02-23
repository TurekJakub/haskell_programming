module Main
  ( main,
  )
where

import Data.Char (isAlpha)
import Text.Printf (printf)

charCountFilter :: [[a]] -> (a -> Bool) -> Int
charCountFilter list filterFunc = foldr (\w acc -> acc + length (filter filterFunc w)) 0 list

calculateVowelPercentage :: String -> Double
calculateVowelPercentage input =
  let filteredWords = filter (\w -> length w > 3) (words input)
   in fromIntegral
        (charCountFilter filteredWords (`elem` "aeiouy"))
        / ( fromIntegral (charCountFilter filteredWords (\w -> isAlpha w))
              / 100
          ) ::
        Double

main :: IO ()
main =
  getContents >>= \i ->
    printf "%.2f%%\n" (calculateVowelPercentage i)
