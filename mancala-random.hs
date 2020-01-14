
import Data.Tuple (swap)
import Data.List (sort)
import System.Random
import Text.Read (readMaybe)

-- ToDos:
-- better propogation of the board size value
-- better rendering of the board
-- organizing the code into separate files and modules
-- create a Board functor/monad?
-- THE COMPUTER CAN PUT IN ZEROS

-- Later ToDos:
-- make the CPU smart
-- timed rendering of the pieces moving by changing CLI output
-- connect to a GUI

main = do
  putStrLn "The game is Mancala. Start!\n"
  gen <- getStdGen
  let b = initBoard
  run b Front gen

run :: Board -> Side -> StdGen -> IO ()
run b wt gen = do
  (spot, newGen) <- fetchSpot b wt gen
  let (newBoard, nextWT, winner) = runTurn wt b spot
  putStrLn $ turnToString newBoard wt spot
  case winner of
    Just s -> do
      putStrLn $ personName s ++ " is the winner! Congratulations!"
      return ()
    Nothing -> run newBoard nextWT newGen

type SideVals = [Int]
--- which side, location (zero indexed), value
type Spot = (Side, Int, Int)
type Spots = [Spot]
type Board = (SideVals, SideVals)
type BoardAcc = ([(Int, Int)], [(Int, Int)])
data Side = Front | Back deriving (Eq, Show)
not' Front = Back
not' Back = Front
type Turn = (Board, Side, Maybe Side)

runTurn :: Side -> Board -> Int -> Turn
runTurn Front = doFrontTurn
runTurn Back = doBackTurn

fetchSpot :: Board -> Side -> StdGen -> IO (Int, StdGen)
fetchSpot b Back gen = return $ getCpuSpot gen
fetchSpot b Front gen = do
  putStrLn $ boardToString b
  putStrLn $ "Put in a move ("++ show sideSize ++ " to 1):"
  input <- getLine
  case (readMaybe input) of
    Just spot -> case (verifyNumberInput b spot) of
      Just errorString -> do
        putStrLn errorString
        fetchSpot b Front gen
      Nothing -> return (sideSize - spot, gen)
    Nothing -> do
      putStrLn $ "Need number input. Put in a move ("++ show sideSize ++ " to 1):"
      fetchSpot b Front gen

verifyNumberInput :: Board -> Int -> Maybe String
verifyNumberInput b spot
  | not (spot `elem` [1..l]) = Just $ "You did not input a value between 1 and " ++ show l
  | (snd b)!!(l - spot) == 0 = Just $ "You cannot move a spot with zero pieces there. Try again."
  | otherwise = Nothing
  where l = (length $ fst b) - 1

doFrontTurn :: Board -> Int -> Turn
doFrontTurn b loc = (nextBoard, wt, winner)
  where spotArray = toSpotArray b
        spot = (Front, loc, (snd b)!!loc)
        nextBoard = fromSpotArray $ map (newSpot spot) spotArray
        wt = nextTurn spot (sideSize + 1)
        winner = whoWon nextBoard

whoWon :: Board -> Maybe Side
whoWon b = if done then Just $ morePieces b else Nothing
  where frontDone = checkPieces $ snd b
        backDone = checkPieces $ fst b
        done = frontDone || backDone

checkPieces :: SideVals -> Bool
checkPieces = (==0) . sum . init

-- TODO: What to do in the case of a Tie?
-- right now, have the CPU win
morePieces :: Board -> Side
morePieces b
  | bp > fp = Back
  | bp < fp = Front
  -- Tie case
  | otherwise = Back
  where bp = last $ fst b
        fp = last $ snd b

toSpotArray :: Board -> Spots
toSpotArray b =
  map (\(loc, val) -> (Back, loc, val)) (zip [0..] (fst b)) ++
  map (\(loc, val) -> (Front, loc, val)) (zip [0..] (snd b))

fromSpotArray :: Spots -> Board
fromSpotArray ss = (fromSideAcc backSpots, fromSideAcc frontSpots)
  where (backSpots, frontSpots) = foldr addSpotToBoard ([], []) ss

fromSideAcc :: [(Int, Int)] -> SideVals
fromSideAcc = snd . unzip . sort

--- TODO: Refactor this function, THERE HAS TO BE A BEETER WAY
addSpotToBoard :: Spot -> BoardAcc -> BoardAcc
addSpotToBoard spot@(Front, loc, val) (backSpots, frontSpots) = (backSpots, (loc, val):frontSpots)
addSpotToBoard spot@(Back, loc, val) (backSpots, frontSpots) = ((loc, val):backSpots, frontSpots)

doBackTurn :: Board -> Int -> Turn
doBackTurn b spot = (flipBoard fb, not' wt, fw)
  where (fb, wt, winner) = doFrontTurn (flipBoard b) spot
        fw = case winner of
          Just s -> Just $ not' s
          Nothing -> Nothing

nextTurn :: Spot -> Int -> Side
nextTurn (wt, loc, val) size = if (loc + val + size + 1) `mod` (size * 2) == 0 then wt else not' wt

getCpuSpot :: StdGen -> (Int, StdGen)
getCpuSpot gen = randomMove sideSize gen

personName :: Side -> String
personName Front = "Player"
personName Back = "CPU"

turnToString :: Board -> Side -> Int -> String
turnToString b wt loc =
  "\n" ++
  personName wt ++ " Move (" ++ show (sideSize - loc) ++ "):" ++
  "\n" ++
  boardToString b ++
  "\n"

sideSize = 8
piecesInSpot = 4
initBoard :: Board
initBoard = makeBoard sideSize piecesInSpot

makeBoard :: Int -> Int -> Board
makeBoard size num = (side, side)
  where side = (take size $ repeat num) ++ [0]

boardToString :: Board -> String
boardToString (cpu, player) =
  (show $ reverse $ cpu) ++ "\n" ++
  "  " ++
  (show $ player)

flipBoard :: Board -> Board
flipBoard = swap

distBetweenSpots :: Spot -> Spot -> Int
distBetweenSpots init@(is, il, _) final@(fs, fl, _)
  | fs == is && fl < il = ((sideSize + 1) * 2) + fl - il
  | fs == is && fl > il = fl - il
  | fs /= is            = sideSize + 1 + fl - il
  | otherwise           = 0

newSpot :: Spot -> Spot -> Spot
newSpot init@(is, il, iv) final@(fs, fl, fv) = (fs, fl, nv)
  where d = distBetweenSpots init final
        size = sideSize + 1
        nv
          | init == final  = 0
          | d + size <= iv = ((iv - d) `div` (size * 2)) + fv + 1
          | d <= iv        = fv + 1
          | d > iv         = fv

randomMove :: Int -> StdGen -> (Int, StdGen)
randomMove size gen = randomR (0,(size - 1)) gen
