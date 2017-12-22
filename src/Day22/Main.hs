module Day22.Main where

import Advent2017.Input (getInputAsString)
import qualified Data.Map as Map

type Grid = Map.Map (Int, Int) NodeState
type Direction = (Int, Int)
type Position = (Int, Int)
type CarrierRule = (NodeState -> NodeState, NodeState -> Direction -> Direction)
type VirusCarrier = (Direction, Position)
data NodeState = Clean | Infected | Weakened | Flagged deriving (Eq)
type GridState = (Grid, VirusCarrier, Int)

addVec2 :: (Int, Int) -> (Int, Int) -> (Int, Int)
addVec2 (a, b) (c, d) = (a + c, b + d)

turnRight :: Direction -> Direction
turnRight (i, j) = (j, -i)

turnLeft :: Direction -> Direction
turnLeft (i, j) = (-j, i)

nodeState :: (Int, Int) -> Grid -> NodeState
nodeState = Map.findWithDefault Clean

part1Carrier :: CarrierRule
part1Carrier = (toggleInfection, turn)
 where
  toggleInfection Infected = Clean
  toggleInfection Clean    = Infected
  toggleInfection _        = error "unreachable"

  turn Infected = turnRight
  turn Clean    = turnLeft
  turn _        = error "unreachable"

part2Carrier :: CarrierRule
part2Carrier = (nextState, turn)
 where
  nextState Clean = Weakened
  nextState Weakened = Infected
  nextState Infected = Flagged
  nextState Flagged = Clean

  turn Clean = turnLeft
  turn Weakened = id
  turn Infected = turnRight
  turn Flagged = turnRight . turnRight

step :: CarrierRule -> GridState -> GridState
step (f, t) (g, (dir, pos), n) = (newG, (newDir, newPos), newN)
 where
  state    = nodeState pos g
  newState = f state
  newG     = Map.insert pos newState g
  newDir   = t state dir
  newPos   = addVec2 pos newDir
  newN     = n + (if newState == Infected then 1 else 0)

parseGrid :: String -> Grid
parseGrid str =
  Map.fromList
    . concatMap
        ( \(i, row) ->
          map (\(j, _) -> ((i - i', j - j'), Infected))
            . filter ((=='#') . snd)
            . zip [0 ..]
            $ row
        )
    . zip [0 ..]
    $ xs
 where
  xs = lines str
  m  = length xs
  n  = length . head $ xs
  i' = m `div` 2
  j' = n `div` 2

main :: IO ()
main = do
  grid <- parseGrid <$> getInputAsString "22"
  let initialState = (grid, ((-1, 0), (0, 0)), 0)
      (_, _, part1)    = iterate (step part1Carrier) initialState !! 10000
      (_, _, part2)    = iterate (step part2Carrier) initialState !! 10000000
  print part1
  print part2
