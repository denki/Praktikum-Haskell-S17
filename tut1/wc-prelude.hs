module Main where

main :: IO ()
main = getContents >>= print . wc

-- | Determines the number of lines, words, and characters in a 'String'.
-- Two consecutive lines are separated by one newline character.
-- Two consecutive words are separated by at least one whitespace character.
wc :: String -> (Int, Int, Int)
wc cs = (length $ lines cs, length $ words cs, length cs)
