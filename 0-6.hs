import Control.Monad
import Data.List (unfoldr, zip5)

isPrime :: Integer -> Bool
isPrime k = null [ x | x <- [2..floor (sqrt (fromIntegral k))], k `mod` x  == 0]

main :: IO ()
main = do
  forM_ (zip5 [2..] [3..] [4..] [5..] [6..]) $ \(i1, i2, i3, i4, i5) -> do
    when (isPrime i1 && isPrime i3 && isPrime i5) $ do
      putStrLn $ show i1 ++ " " ++ show i3 ++ " " ++ show i5
