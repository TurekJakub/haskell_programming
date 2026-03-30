import Data.List.NonEmpty (NonEmpty, nonEmpty, nub)
import Data.Maybe (fromJust)

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact

data LogoShape = LogoShape
  { picture :: Picture
  , position :: (Float, Float)
  , shapeColor :: Color
  , isSelected :: Bool
  , isPickedUp :: Bool
  , occupiedQuads :: [QuadRecord]
  , shapeType :: ShapeType
  } deriving (Show)

newtype WorldSate = WorldState
  { shapes :: [LogoShape]
  } deriving (Show)

data QuadRecord = QuadRecord
  { blockXIndex :: Float
  , blockYIndex :: Float
  , quadIndex :: Int
  } deriving (Show, Eq)

data RotationDirection
  = Clockwise Float
  | CounterClockwise Float

data ShapeType
  = Triangle
  | LargeTriangle
  | Block
  | Parallelogram
  deriving (Show)

defaultBlockSize :: Float
defaultBlockSize = 100

getShapeColor :: LogoShape -> Color
getShapeColor shape
  | isSelected shape = light $ light $ shapeColor shape
  | otherwise = shapeColor shape

colorPallet :: [Color]
colorPallet =
  [red, blue, orange, rose, aquamarine, green, black, chartreuse, magenta]

getNextColor :: Color -> Color
getNextColor current =
  case take 2 $ dropWhile (/= current) $ cycle colorPallet of
    (_:h:_) -> h
    _ -> red

rect :: LogoShape
rect =
  LogoShape
    (rectangleSolid 100 100)
    (100, 100)
    red
    False
    False
    [QuadRecord 1 1 0, QuadRecord 1 1 1, QuadRecord 1 1 2, QuadRecord 1 1 3]
    Block

rect2 :: LogoShape
rect2 =
  LogoShape
    (rectangleSolid 100 100)
    (500, 100)
    blue
    False
    False
    [QuadRecord 5 1 0, QuadRecord 5 1 1, QuadRecord 5 1 2, QuadRecord 5 1 3]
    Block

triangle :: LogoShape
triangle =
  LogoShape
    (polygon [(50.0, -50.0), (50.0, 50.0), (-50.0, 50.0)])
    (400, 100)
    rose
    False
    False
    [QuadRecord 4 1 2, QuadRecord 4 1 3]
    Triangle

triangle2 :: LogoShape
triangle2 =
  LogoShape
    (rotate 270 $ polygon [(50.0, -50.0), (50.0, 50.0), (-50.0, 50.0)])
    (200, 100)
    black
    False
    False
    [QuadRecord 2 1 3, QuadRecord 2 1 4]
    Triangle

triangleBig :: LogoShape
triangleBig =
  LogoShape
    (polygon [(100, -100), (0, 0), (-100, -100)])
    (350, 150)
    orange
    False
    False
    [QuadRecord 3 1 1, QuadRecord 3 1 2, QuadRecord 4 1 1, QuadRecord 4 1 4]
    LargeTriangle

parallelogram :: LogoShape
parallelogram =
  LogoShape
    (polygon [(0, 0), (100, 0), (200, 100), (100, 100)])
    (150, 50)
    green
    False
    False
    [QuadRecord 2 1 1, QuadRecord 2 1 2, QuadRecord 3 1 3, QuadRecord 3 1 4]
    Parallelogram

initialWorld :: WorldSate
initialWorld =
  WorldState [rect, rect2, triangle, triangle2, triangleBig, parallelogram]

draw :: LogoShape -> Picture
draw x = Color (getShapeColor x) $ uncurry Translate (position x) $ picture x

moveQuad :: Float -> Float -> QuadRecord -> QuadRecord
moveQuad y x quad =
  quad
    { blockXIndex = x / defaultBlockSize + blockXIndex quad
    , blockYIndex = y / defaultBlockSize + blockYIndex quad
    }

mapTuple :: (t -> b) -> (t, t) -> (b, b)
mapTuple f (x, y) = (f x, f y)

rotateShapeQuads :: RotationDirection -> LogoShape -> [QuadRecord]
rotateShapeQuads dir shape =
  let (x, y) = mapTuple (/ defaultBlockSize) $ position shape
   in map (rotateQuad x y dir) $ occupiedQuads shape

rotateQuad :: Float -> Float -> RotationDirection -> QuadRecord -> QuadRecord
rotateQuad px py dir quad =
  let newIdx = quadIndexMap dir $ quadIndex quad
   in case dir of
        Clockwise _ ->
          quad
            { blockXIndex = px + (blockYIndex quad - py)
            , blockYIndex = py - (blockXIndex quad - px)
            , quadIndex = newIdx
            }
        CounterClockwise _ ->
          quad
            { blockXIndex = px - (blockYIndex quad - py)
            , blockYIndex = py + (blockXIndex quad - px)
            , quadIndex = newIdx
            }

quadIndexMap :: (Eq a, Num a) => RotationDirection -> a -> a
quadIndexMap (Clockwise _) idx
  | idx == 1 = 4
  | idx == 2 = 1
  | idx == 3 = 2
  | idx == 4 = 3
  | otherwise = idx
quadIndexMap (CounterClockwise _) idx
  | idx == 1 = 2
  | idx == 2 = 3
  | idx == 3 = 4
  | idx == 4 = 1
  | otherwise = idx

quadIndexFlipMap :: (Eq a, Num a) => a -> a
quadIndexFlipMap idx
  | idx == 1 = 3
  | idx == 3 = 1
  | otherwise = idx

flipQuadInx :: QuadRecord -> QuadRecord
flipQuadInx quad = quad {quadIndex = quadIndexFlipMap $ quadIndex quad}

updateQuadRotate :: RotationDirection -> LogoShape -> LogoShape
updateQuadRotate dir shape =
  let (x, y) = mapTuple (/ defaultBlockSize) $ position shape
   in shape {occupiedQuads = map (rotateQuad x y dir) $ occupiedQuads shape}

move :: Float -> Float -> LogoShape -> LogoShape
move sx sy s
  | isPickedUp s =
    let (x, y) = position s
     in s
          { position = (x + sx, y + sy)
          , occupiedQuads = map (moveQuad sy sx) $ occupiedQuads s
          }
  | otherwise = s

select :: [LogoShape] -> [LogoShape]
select list =
  case list of
    (h:newHead:rest) ->
      (newHead {isSelected = True})
        : rest
        ++ [h {isSelected = False, isPickedUp = False}]
    _ -> list

pickup :: [LogoShape] -> [LogoShape]
pickup (h:xs)
  | isPickedUp h =
    if checkCollisions $ fromJust $ nonEmpty (collectOccupied (h : xs))
      then h {isPickedUp = False, isSelected = False} : xs
      else h : xs
  | isSelected h = h {isPickedUp = True} : xs
  | otherwise = h : xs
pickup [] = []

changeColor :: [LogoShape] -> [LogoShape]
changeColor (x:xs)
  | isSelected x = x {shapeColor = getNextColor $ shapeColor x} : xs
  | otherwise = x : xs
changeColor [] = []

rotateShape :: RotationDirection -> [LogoShape] -> [LogoShape]
rotateShape dir (h:xs)
  | isPickedUp h =
    let newH = updateQuadRotate dir h
     in case dir of
          Clockwise ang -> newH {picture = rotate ang $ picture newH} : xs
          CounterClockwise ang ->
            newH {picture = rotate (-ang) $ picture newH} : xs
  | otherwise = h : xs
rotateShape _ [] = []

flipShape :: [LogoShape] -> [LogoShape]
flipShape (x:xs)
  | isPickedUp x = flipHelper x : xs
  | otherwise = x : xs
flipShape [] = []

flipHelper :: LogoShape -> LogoShape
flipHelper shape =
  case shapeType shape of
    Triangle ->
      shape
        { occupiedQuads = map flipQuadInx $ occupiedQuads shape
        , picture = scale 1 (-1) (picture shape)
        }
    _ ->
      let newShape =
            shape {occupiedQuads = rotateShapeQuads (Clockwise 90) shape}
       in newShape
            { picture = scale 1 (-1) (picture shape)
            , occupiedQuads = rotateShapeQuads (Clockwise 90) newShape
            }

collectOccupied :: [LogoShape] -> [QuadRecord]
collectOccupied = concatMap occupiedQuads

checkCollisions :: Eq a => NonEmpty a -> Bool
checkCollisions occupied = length (nub occupied) == length occupied

{- This function draws the world (integer `n`) as a Gloss `Picture` type.
 - (see the documentation for the Picture type on Hoogle.) -}
drawWorld :: WorldSate -> Picture
drawWorld n = Pictures $ map draw (shapes n)

{- This function changes the world (integer `n`) based on an incoming event, in
 - our case arrow keys being pressed.a -}
handleEvent :: Event -> WorldSate -> WorldSate
handleEvent (EventKey (SpecialKey KeyLeft) Down _ _) n = n {shapes = map (move (-defaultBlockSize) 0) (shapes n)}
handleEvent (EventKey (SpecialKey KeyRight) Down _ _) n = n {shapes = map (move defaultBlockSize 0) (shapes n)}
handleEvent (EventKey (SpecialKey KeyUp) Down _ _) n = n {shapes = map (move 0 defaultBlockSize) (shapes n)}
handleEvent (EventKey (SpecialKey KeyDown) Down _ _) n = n {shapes = map (move 0 (-defaultBlockSize)) (shapes n)}
handleEvent (EventKey (SpecialKey KeyTab) Down _ _) n = n {shapes = select (shapes n)}
handleEvent (EventKey (SpecialKey KeySpace) Down _ _) n = n {shapes = pickup (shapes n)}
handleEvent (EventKey (Char 'x') Down _ _) n = n {shapes = changeColor (shapes n)}
handleEvent (EventKey (Char 'r') Down _ _) n = n {shapes = rotateShape (Clockwise 90) (shapes n)}
handleEvent (EventKey (Char 'z') Down _ _) n = n {shapes = rotateShape (CounterClockwise 90) (shapes n)}
handleEvent (EventKey (Char 'v') Down _ _) n = n {shapes = flipShape (shapes n)}
handleEvent _ n = n -- we ignore all other events

{- This function is supposed to update the world regularly after some time
 - interval passes. The parameter would be the time difference to cover with
 - the update (we discard it with `_`), and the function would be able to
 - change the initial state (and we don't change anything by returning `id`).
 -
 - Unless you want actual animated things, you can leave this as is.
 -}
updateWorld :: p -> a -> a
updateWorld _ = id

{- Function `play` from gloss connects the functions for managing and drawing
 - the world state and runs them on the initial state, with a selected
 - background color and framerate. All other things are handled by the Gloss
 - library. -}
main :: IO ()
main = play FullScreen white 25 initialWorld drawWorld handleEvent updateWorld
