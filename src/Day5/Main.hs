module Day5.Main where

type Instruction = Int
type Machine = ([Instruction], [Instruction])

makeMachine :: [Instruction] -> Machine
makeMachine (x:xs) = ([x], xs)

terminated :: Machine -> Bool
terminated ([], _) = True
terminated _       = False

incCurrent :: Machine -> Machine
incCurrent (x:xs, ys) = (succ x : xs, ys)

moveForward :: Machine -> Machine
moveForward (xs, []  ) = ([], xs)
moveForward (xs, y:ys) = (y : xs, ys)

moveBackward :: Machine -> Machine
moveBackward (x:xs, ys) = (xs, x : ys)

advance :: Machine -> Machine
advance m@(x:xs, ys) | x >= 0    = iterate moveForward (incCurrent m) !! x
                     | otherwise = iterate moveBackward (incCurrent m) !! (-x)

runMachine :: Machine -> [Machine]
runMachine = iterate advance

solve :: String -> IO ()
solve input = do
  let instructions = map read . lines $ input
      initialState = makeMachine instructions
      states       = takeWhile (not . terminated) . runMachine $ initialState
  print . length $ states
