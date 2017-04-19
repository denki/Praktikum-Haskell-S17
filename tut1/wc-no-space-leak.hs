{-# LANGUAGE BangPatterns #-}

module Main where

import qualified Data.ByteString.Lazy as B
import Data.Attoparsec.ByteString.Char8 (isSpace_w8)
import Data.Char
import Data.List (foldl')
import Data.Word (Word8)


data Acc e a f = Acc
  { initAcc   :: a
  , stepAcc   :: a -> e -> a
  , finishAcc :: a -> f
  }


-- | Strictly construct pair.
(<!>) :: a -> b -> (a, b)
infixl 0 <!>
x <!> y = x `seq` y `seq` (x, y)

lengthAcc :: Acc a Int Int
lengthAcc = Acc 0 (\ r _ -> r + 1) id

wordsAcc :: Acc Word8 (Int, Bool) Int
wordsAcc = Acc (0, True) step fst
  where step acc@(cnt, True ) c = if isSpace_w8 c then acc else cnt + 1 <!> False
        step acc@(cnt, False) c = if isSpace_w8 c then (cnt, True) else acc

newlineAcc :: (Eq a, Num a) => Acc a Int Int
newlineAcc = Acc 0 step id
  where step r c = if fromIntegral (ord '\n') == c then r + 1 else r

(|!|) :: Acc e a1 f1 -> Acc e a2 f2 -> Acc e (a1, a2) (f1, f2)
acc1 |!| acc2 = Acc (initAcc acc1, initAcc acc2) step finish
  where step   (a1, a2) e = stepAcc   acc1 a1 e <!> stepAcc   acc2 a2 e
        finish (a1, a2)   = finishAcc acc1 a1   <!> finishAcc acc2 a2

foldAcc :: Acc e a f -> [e] -> f
foldAcc a = finishAcc a . foldl' (stepAcc a) (initAcc a)

main :: IO ()
main
  = B.getContents
  >>= print . foldAcc (newlineAcc |!| wordsAcc |!| lengthAcc) . B.unpack
