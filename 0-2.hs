import Control.Monad
import Data.List (unfoldr)
import Data.Maybe (listToMaybe)

isPrime' :: Integer -> Bool
isPrime' k = null [ x | x <- [2..floor (sqrt (fromIntegral k))], k `mod` x  == 0]

isPrime :: Integer -> String
isPrime n
  | isPrime' n = "prime"
  | otherwise  = "not prime"

formula1 :: Integer -> Integer
formula1 n = 3^n - 1

formula2 :: Integer -> Integer
formula2 n = 3^n - 2^n

main :: IO ()
main = do
  forM_ [2..50] $ \i -> do
    print $ show i ++ ". " ++
      "3^n - 1: " ++ isPrime (formula1 i) ++
      " | " ++
      "3^n - 2^n: " ++ isPrime (formula2 i)
