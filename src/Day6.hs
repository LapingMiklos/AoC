module Day6 where

solutionCount :: (Int, Int) -> Int
solutionCount (time, record) = 1 + time - 2 * until (\x -> x * (time - x) >= record) (+ 1) 0

main = do
  print $ foldl (*) 1 $ map solutionCount [(56, 499), (97, 2210), (77, 1097), (93, 1440)]
  print $ solutionCount (56977793, 499221010971440)