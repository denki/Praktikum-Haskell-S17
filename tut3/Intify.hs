import Prelude hiding (mapM)

import Control.DeepSeq
import Control.Monad.State.Lazy hiding (mapM)
import Data.List (foldl')
import Data.Traversable (mapM)
import Data.Tree
import Data.Tuple (swap)
import System.Environment (getArgs, getProgName)
import System.Random


intify :: Eq a => [Tree a] -> ([Tree Int], [(a, Int)])
intify ts0 = runState (intifyList ts0) []
  where
    intifyList :: Eq a => [Tree a] -> State [(a, Int)] [Tree Int]
    intifyList []       = return []
    intifyList (t : ts) = do
      t' <- intifyTree t
      ts' <- intifyList ts
      t' `seq` return (t' : ts')

    intifyTree :: Eq a => Tree a -> State [(a, Int)] (Tree Int)
    intifyTree (Node x ts) = do
      x' <- lookupInsert x
      ts' <- intifyList ts
      x' `seq` return (Node x' ts')

    lookupInsert :: Eq a => a -> State [(a, Int)] Int
    lookupInsert x = do
      assocs <- get
      case lookup x assocs of
        Just i -> return i
        Nothing -> let i = case assocs of { [] -> 0; (_, i') : _ -> i' + 1 }
                   in i `seq` put ((x, i): assocs) >> return i


-- Tests ---------------------------------------------------------------------

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["correctness"] -> do
      putStr "Testing []           ... "
      testCorrectness ([] :: [Tree ()])
      putStr "Testing random trees ... "
      testCorrectness $ genTreesRnd 0 ('a', 'z') 9 1000
    ["heap0"]  -> testHeap  $ genTrees 1000
    ["heap1"]  -> testHeap  $ genTreesRnd 0 ('a', 'z') 9 20000
    ["stack0"] -> testStack $ genTrees 500
    ["stack1"] -> testStack $ genTreesRnd 0 ('a', 'z') 9 20000
    _ -> printHelp


printHelp :: IO ()
printHelp = getProgName >>= \ progName -> mapM_ putStrLn
  [ progName ++ " <CMD>"
  , "  where <CMD> is one of"
  , "    correctness   : Do a simple correctness test."
  , "    heap0, heap1  : These shall not consume much memory, otherwise you"
  , "                    are too lazy. Try with +RTS -M32M -RTS."
  , "    stack0, stack1: These shall not lead to stack space overflows."
  ]


testCorrectness :: Eq a => [Tree a] -> IO ()
testCorrectness ts = do
  let (ts', assocs) = intify ts
  case mapM (mapM (flip lookup (map swap assocs))) ts' of
    Nothing -> putStrLn "Error: The returned association list is incomplete."
    Just ts'' | ts /= ts'' -> putStrLn "Error: The returned trees or \
                                       \association list are wrong."
              | otherwise  -> putStrLn "Test passed."


testHeap :: (NFData a, Eq a) => [Tree a] -> IO ()
testHeap
  = print
  . foldl' (+) 0
  . concatMap flatten
  . fst
  . intify
  . listDeepseq


testStack :: (NFData a, Eq a) => [Tree a] -> IO ()
testStack
  = print
  . last
  . concatMap flatten
  . fst
  . intify
  . listDeepseq


genTrees :: Int -> [Tree Int]
genTrees cnt
  = unfoldForest
      ( \ n ->
        ( n
        , if n > 1 then let n2 = n `div` 2 in [n2, n - n2] else []
        )
      )
      [1 :: Int .. cnt]
  ++ [Node 99999999 []]


genTreesRnd :: Random a => Int -> (a, a) -> Int -> Int -> [Tree a]
genTreesRnd seed bounds maxChildren cnt
  = evalState
      (replicateM cnt (genTreeRnd bounds maxChildren))
      (mkStdGen seed)


genTreeRnd :: (Random a, RandomGen g) => (a, a) -> Int -> State g (Tree a)
genTreeRnd bounds c = do
  r <- state $ randomR bounds
  c' <- state $ randomR (0, c - 1)
  ts <- replicateM c' $ genTreeRnd bounds c'
  return (Node r ts)


listDeepseq :: NFData a => [a] -> [a]
listDeepseq = foldr (\ x xs -> x `deepseq` (x : xs)) []
