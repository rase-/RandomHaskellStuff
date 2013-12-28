module StringStuff
( recEdit
, mRecEdit
, lazyEdit
, stEdit
) where

import Data.Array
import Control.Monad
import Control.Monad.ST
import Data.Array.ST
import Data.Function.Memoize

matchMismatch :: Array Int Char -> Array Int Char -> Int -> Int -> Int
matchMismatch xs ys i j
  | xs ! i == ys ! j = 0
  | otherwise = 1

recEdit :: String -> String -> Int
recEdit xs ys = dist m n
  where (m,n) = (length xs, length ys)
        axs = array (1,m) $ zip [1..] xs
        ays = array (1,n) $ zip [1..] ys
        dist i j
          | min i j == 0 = max i j
          | otherwise = minimum [1 + dist (i - 1) j, 1 + dist i (j - 1), dist (i - 1) (j - 1) + matchMismatch axs ays i j]

mRecEdit :: String -> String -> Int
mRecEdit xs ys = mdist m n
  where (m,n) = (length xs, length ys)
        axs = array (1,m) $ zip [1..] xs
        ays = array (1,n) $ zip [1..] ys
        dist :: (Int -> Int -> Int) -> Int -> Int -> Int
        dist f i j
          | min i j == 0 = max i j
          | otherwise = minimum [1 + f (i - 1) j, 1 + f i (j - 1), f (i - 1) (j - 1) + matchMismatch axs ays i j]
        mdist :: Int -> Int -> Int
        mdist = memoFix dist


lazyEdit :: String -> String -> Int
lazyEdit xs ys = mat ! (m,n)
  where mat = array ((0,0),(m,n)) [((i,j), dist i j) | i <- [0..m], j <- [0..n]]
        (m,n) = (length xs, length ys)
        axs = array (1,m) $ zip [1..] xs
        ays = array (1,n) $ zip [1..] ys
        dist 0 j = j
        dist i 0 = i
        dist i j = minimum [mat ! (i - 1, j) + 1, mat ! (i, j - 1) + 1, mat ! (i - 1, j - 1) + (matchMismatch axs ays i j)]

stEdit :: String -> String -> Int
stEdit xs ys = runST $ do mat <- newArray ((0,0),(m,n)) 0 :: ST s (STArray s (Int,Int) Int)
                          mapM_ (\i -> do writeArray mat (i,0) i) [1..m]
                          mapM_ (\j -> do writeArray mat (0,j) j) [1..n]
                          forM_ [(i,j) | i <- [1..m], j <- [1..n]] (\p@(i,j) ->
                            do a <- readArray mat (i - 1, j)
                               b <- readArray mat (i, j - 1)
                               c <- readArray mat (i - 1, j - 1)
                               let val = minimum [1 + a, 1 + b, c + matchMismatch axs ays i j]
                               writeArray mat p val
                               return val)
                          x <- readArray mat (m,n)
                          return x
  where (m,n) = (length xs, length ys)
        axs = array (1,m) $ zip [1..] xs
        ays = array (1,n) $ zip [1..] ys
