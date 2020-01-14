
// -- Later ToDos:
// -- make the CPU smart
// -- timed rendering of the pieces moving by changing CLI output
// -- connect to a GUI

// main = do
//   putStrLn "The game is Mancala. Start!\n"
//   gen <- getStdGen
//   let b = initBoard
//   run b Front gen
const SIDESIZE = 8;
const PIECESINSPOT = 4;
const FRONT = 1;
const BACK = 0;
function main () {
  alert('The game is Mancala. Start!');
  const b = Array(2).fill().map(() => Array(SIDESIZE + 1).fill(4));
  b[BACK][SIDESIZE] = 0;
  b[FRONT][SIDESIZE] = 0;
  run(b, FRONT);
}

// run :: Board -> Side -> StdGen -> IO ()
// run b wt gen = do
//   (spot, newGen) <- fetchSpot b wt gen
//   let (newBoard, nextWT, winner) = runTurn wt b spot
//   putStrLn $ turnToString newBoard wt spot
//   case winner of
//     Just s -> do
//       putStrLn $ personName s ++ " is the winner! Congratulations!"
//       return ()
//     Nothing -> run newBoard nextWT newGen
function run(b, wt) {
  const spot = fetchSpot(b, wt);
  if(!spot) {
    return
  }
  const [newBoard, nextWT, winner] = runTurn(wt)(b, spot)
  alert(turnToString(newBoard, wt, spot))
  if(winner) {
    alert(personName(s) + " is the winner! Congratulations!")
  } else {
    run(newBoard, nextWT)
  }
}

function not(side) {
  return 1 - side;
}

// runTurn :: Side -> Board -> Int -> Turn
// runTurn Front = doFrontTurn
// runTurn Back = doBackTurn
function runTurn(side) {
  if(side) {
    return doFrontTurn
  } else {
    return doBackTurn
  }
}

// fetchSpot :: Board -> Side -> StdGen -> IO (Int, StdGen)
// fetchSpot b Back gen = return $ getCpuSpot gen
// fetchSpot b Front gen = do
//   putStrLn $ boardToString b
//   putStrLn $ "Put in a move ("++ show sideSize ++ " to 1):"
//   input <- getLine
//   case (readMaybe input) of
//     Just spot -> case (verifyNumberInput b spot) of
//       Just errorString -> do
//         putStrLn errorString
//         fetchSpot b Front gen
//       Nothing -> return (sideSize - spot, gen)
//     Nothing -> do
//       putStrLn $ "Need number input. Put in a move ("++ show sideSize ++ " to 1):"
//       fetchSpot b Front gen
function fetchSpot(b, side) {
  if(side) {
    alert(boardToString(b))
    const input = prompt(`Put in a move (${SIDESIZE} to 1):`)
    if(!input) {
      alert('Looks like you don\'t want to play anymore. Goodbye!')
      return false
    }
    const spot = parseInt(input)
    if(isNaN(spot)) {
      alert(`Need number input. Put in a move (${SIDESIZE} to 1):`);
    } else {
      const error = verifyNumberInput(b, spot)
      if(error) {
        alert(error)
        fetchSpot(b, side)
      } else {
        return SIDESIZE - spot
      }
    }
  } else {
    return getCpuSpot()
  }
}

// verifyNumberInput :: Board -> Int -> Maybe String
// verifyNumberInput b spot
//   | not (spot `elem` [1..l]) = Just $
//   | (snd b)!!(l - spot) == 0 = Just $ "You cannot move a spot with zero pieces there. Try again."
//   | otherwise = Nothing
//   where l = (length $ fst b) - 1
function verifyNumberInput(b, spot) {
  if(!(0 < spot && spot <= SIDESIZE)) {
    return "You did not input a value between 1 and " + SIDESIZE
  } else if(b[FRONT][SIDESIZE - spot] === 0) {
    return "You cannot move a spot with zero pieces there. Try again."
  }
}

// doFrontTurn :: Board -> Int -> Turn
// doFrontTurn b loc = (nextBoard, wt, winner)
//   where spotArray = toSpotArray b
//         spot = (Front, loc, (snd b)!!loc)
//         nextBoard = fromSpotArray $ map (newSpot spot) spotArray
//         wt = nextTurn spot (sideSize + 1)
//         winner = whoWon nextBoard
function doFrontTurn(b, loc) {
  const spotArray = toSpotArray(b)
  const spot = [FRONT, loc, b[FRONT][loc]]
  const nextBoard = fromSpotArray(spotArray.map((final) => newSpot(spot, final)))
  const wt = nextTurn(spot, (SIDESIZE + 1))
  const winner = whoWon(nextBoard)
  return [nextBoard, wt, winner]
}

// whoWon :: Board -> Maybe Side
// whoWon b = if done then Just $ morePieces b else Nothing
//   where frontDone = checkPieces $ snd b
//         backDone = checkPieces $ fst b
//         done = frontDone || backDone
function whoWon(b) {
  const frontDone = checkPieces(b[FRONT])
  const backDone = checkPieces(b[BACK])
  const done = frontDone || backDone
  if(done) {
    return morePieces(b)
  }
}


// checkPieces :: SideVals -> Bool
// checkPieces = (==0) . sum . init
function checkPieces(side) {
  return side.slice(0,SIDESIZE - 1).reduce((ac, n) => ac + n) === 0
}

// -- TODO: What to do in the case of a Tie?
// -- right now, have the CPU win
// morePieces :: Board -> Side
// morePieces b
//   | bp > fp = Back
//   | bp < fp = Front
//   -- Tie case
//   | otherwise = Back
//   where bp = last $ fst b
//         fp = last $ snd b
function morePieces(b) {
  const bp = b[BACK][SIDESIZE]
  const fp = b[FRONT][SIDESIZE]
  if(bp > fp) {
    return BACK
  } else if(bp < fp) {
    return FRONT
  } else {
    return BACK
  }
}

// toSpotArray :: Board -> Spots
// toSpotArray b =
//   map (\(loc, val) -> (Back, loc, val)) (zip [0..] (fst b)) ++
//   map (\(loc, val) -> (Front, loc, val)) (zip [0..] (snd b))
function toSpotArray(b) {
  return [...b[BACK].map((val, loc) => [BACK, loc, val]), ...b[FRONT].map((val, loc) => [FRONT, loc, val])]
}

// fromSpotArray :: Spots -> Board
// fromSpotArray ss = (fromSideAcc backSpots, fromSideAcc frontSpots)
//   where (backSpots, frontSpots) = foldr addSpotToBoard ([], []) ss
function fromSpotArray(ss) {
  const [backSpots, frontSpots] = ss.reduce(addSpotToBoard, [[],[]])
  return [fromSideAcc(backSpots), fromSideAcc(frontSpots)]
}

// fromSideAcc :: [(Int, Int)] -> SideVals
// fromSideAcc = snd . unzip . sort
function fromSideAcc(spots) {
  return spots.sort(([aloc], [bloc]) => Math.sign(aloc - bloc)).map(([_, val]) => val)
}

// --- TODO: Refactor this function, THERE HAS TO BE A BEETER WAY
// addSpotToBoard :: Spot -> BoardAcc -> BoardAcc
// addSpotToBoard spot@(Front, loc, val) (backSpots, frontSpots) = (backSpots, (loc, val):frontSpots)
// addSpotToBoard spot@(Back, loc, val) (backSpots, frontSpots) = ((loc, val):backSpots, frontSpots)
function addSpotToBoard([backSpots, frontSpots], spot) {
  const [side, loc, val] = spot
  if(side) {
    return [backSpots, [...frontSpots, [loc, val]]]
  } else {
    return [[...backSpots, [loc, val]], frontSpots]
  }
}

// doBackTurn :: Board -> Int -> Turn
// doBackTurn b spot = (flipBoard fb, not' wt, fw)
//   where (fb, wt, winner) = doFrontTurn (flipBoard b) spot
//         fw = case winner of
//           Just s -> Just $ not' s
//           Nothing -> Nothing
function doBackTurn(b, spot) {
  const [fb, wt, winner] = doFrontTurn(flipBoard(b), spot)
  let fw
  if(winner === BACK || winner === FRONT){
    fw = not(winner)
  }
  return [flipBoard(fb), not(wt), fw]
}

// nextTurn :: Spot -> Int -> Side
// nextTurn (wt, loc, val) size = if (loc + val + size + 1) `mod` (size * 2) == 0 then wt else not' wt
function nextTurn([wt, loc, val], size) {
  if ((loc + val + size + 1) % (size * 2) == 0) {
    return wt
  } else {
    return not(wt)
  }
}

// getCpuSpot :: StdGen -> (Int, StdGen)
// getCpuSpot gen = randomMove sideSize gen
const getCpuSpot = () => Math.round(Math.random() * (SIDESIZE - 1)) + 1

// personName :: Side -> String
// personName Front = "Player"
// personName Back = "CPU"
const personName = side => ["CPU", "Player"][side]

// turnToString :: Board -> Side -> Int -> String
// turnToString b wt loc =
//   "\n" ++
//   personName wt ++ " Move (" ++ show (sideSize - loc) ++ "):" ++
//   "\n" ++
//   boardToString b ++
//   "\n"
const turnToString = (b, wt, loc) => `
${personName(wt)} Move (${SIDESIZE - loc}):

${boardToString(b)}
`

// boardToString :: Board -> String
// boardToString (cpu, player) =
//   (show $ reverse $ cpu) ++ "\n" ++
//   "  " ++
//   (show $ player)
function boardToString(b) {
  return `[${[...b[BACK]].reverse().join(',')}]
  \n[${b[FRONT].join(',')}]`
}

// flipBoard :: Board -> Board
// flipBoard = swap
const flipBoard = ([back, front]) => [front, back]

// distBetweenSpots :: Spot -> Spot -> Int
// distBetweenSpots init@(is, il, _) final@(fs, fl, _)
//   | fs == is && fl < il = ((sideSize + 1) * 2) + fl - il
//   | fs == is && fl > il = fl - il
//   | fs /= is            = sideSize + 1 + fl - il
//   | otherwise           = 0
function distBetweenSpots(init, final) {
  const [is, il] = init
  const [fs, fl] = final
  if(fs === is && fl < il) {
    return ((SIDESIZE + 1) * 2) + fl - il
  } else if(fs === is && fl > il) {
    return fl - il
  } else if(fs !== is) {
    return SIDESIZE + 1 + fl - il
  } else {
    return 0
  }
}

// newSpot :: Spot -> Spot -> Spot
// newSpot init@(is, il, iv) final@(fs, fl, fv) = (fs, fl, nv)
//   where d = distBetweenSpots init final
//         size = sideSize + 1
//         nv
//           | init == final  = 0
//           | d + size <= iv = ((iv - d) `div` (size * 2)) + fv + 1
//           | d <= iv        = fv + 1
//           | d > iv         = fv
function newSpot(init, final) {
  const [is, il, iv] = init
  const [fs, fl, fv] = final
  const d = distBetweenSpots(init, final)
  const size = SIDESIZE + 1
  let nv;
  if(is === fs && il === fl && iv === fv) {
    nv = 0
  } else if(d + size <= iv) {
    nv = Math.floor((iv - d) / (size * 2)) + fv + 1
  } else if(d <= iv) {
    nv = fv + 1
  } else if(d > iv) {
    nv = fv
  }
  return [fs, fl, nv]
}

// randomMove :: Int -> StdGen -> (Int, StdGen)
// randomMove size gen = randomR (0,(size - 1)) gen
main();