-- Copyright 2023 Google LLC
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--     http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Use sortOn" #-}

module Main where

import Control.Monad (unless)
import qualified Data.Array as A
import Data.Foldable (foldl')
import Data.Function ((&))
import qualified Data.IntMap.Strict as IM
import qualified Data.Set as S
import Data.Word (Word32, Word8)
import Debug.Trace
import qualified Draw as D
import qualified Textures as Tex
import Lens.Micro ((^.))
import Linear (V4(..))
import SDL
import SDL.Primitive
import qualified SDL.Font as Font
import System.Random (StdGen, getStdGen, randomRs)
import Text.Show.Pretty (pPrint)
import qualified Data.Vector as Vec
import Data.Ord (comparing, Down(..))
import Data.List (sortBy)
import qualified Data.Sequence as Seq
import qualified Data.Text as T

import qualified Foreign.Ptr as Ptr
import qualified SDL.Raw.Video as Raw
import qualified SDL.Raw.Types as Raw
import qualified SDL.Raw as Raw
import Foreign.Storable (peek, poke)

{- VECTORS -}
i2f :: Integral a => a -> Float
i2f = fromIntegral

i2i :: (Integral a, Integral b) => a -> b
i2i = fromIntegral

angleBetween :: V2 Float -> V2 Float -> Float
angleBetween u v = atan2 (det22 (V2 u v)) (dot u v)

rotateV2 :: Float -> V2 Float -> V2 Float
rotateV2 a v = V2 (V2 (cos a) (-sin a)) (V2 (sin a) (cos a)) !* v

{- MAP -}
type WallType = Int

type Map = A.Array (Int, Int) WallType

parseMap :: [String] -> Map
parseMap rows =
  A.array ((0, 0), (w - 1, h - 1)) $ concat $ zipWith parseRow [0 ..] rows
  where
    w = length (head rows)
    h = length rows
    parseRow j row = zipWith (parseCell j) [0 ..] row
    parseCell j i cell =
      ( (i, j)
      , if cell == ' '
          then 0
          else read [cell])

testMap :: [String]
testMap =
  [ "111111111111111111111111111111"
  , "1                            1"
  , "1   333                333   1"
  , "1   333                333   1"
  , "1                            1"
  , "1111111111111111 1111111111111"
  , "1      1       1 1  1        1"
  , "1      1       1 1     3  3  1"
  , "1 111111       1 1  1        1"
  , "1      2       1 1111  3  3  1"
  , "1      1       1    1        1"
  , "1      1       111  1  3  3  1"
  , "1      1         1  1        1"
  , "1      111111111 1  1  3  3  1"
  , "1                1  1        1"
  , "1              1 1     3  3  1"
  , "1 33   111111  1 1  1        1"
  , "1      1    1  1 1  1  3  3  1"
  , "11111111    1  1 1  1        1"
  , "1      1       1 1  1111111111"
  , "1 3    1    1  1 1  1        1"
  , "1      1    1  1 1  1        1"
  , "1      111111  1    1        1"
  , "1              111111        1"
  , "1      11  11       4        1"
  , "1      1    1  111111   33   1"
  , "1      1    1  1    1   33   1"
  , "1      1    1       1        1"
  , "1      1    1       1        1"
  , "111111111111111111111111111111"
  ]

walkable :: Map -> V2 Float -> Bool
walkable m pos =
  x >= 0 &&
  x <= w && y >= 0 && y <= h && (m A.! (x, y) == 0 || m A.! (x, y) == 2)
  where
    x = floor (pos ^. _x)
    y = floor (pos ^. _y)
    (_, (w, h)) = A.bounds m

{- GAME STATE -}
data Player =
  Player
    { playerPos :: !(V2 Float)
    , playerDir :: !(V2 Float)
    , keysPressed :: !(S.Set Keycode)
    }
  deriving (Show)

data State =
  State
    { worldMap :: !Map
    , worldTime :: !Float
    , lastDts :: !(Seq.Seq Float)
    , players :: !(IM.IntMap Player)
    }
  deriving (Show)

{- EVENT HANDLING -}
data SimpleEvent
  = EMotion Float Float
  | EKeyDown Keycode
  | EKeyUp Keycode
  | EUnknown
  deriving (Show)

eventToSimpleEvent :: Event -> SimpleEvent
eventToSimpleEvent event = convert (eventPayload event)
  where
    convert (MouseMotionEvent (MouseMotionEventData {mouseMotionEventRelMotion = V2 x y})) =
      EMotion (i2f x) (i2f y)
    convert (KeyboardEvent (KeyboardEventData { keyboardEventKeyMotion = motion
                                              , keyboardEventRepeat = False
                                              , keyboardEventKeysym = Keysym {keysymKeycode = keycode}
                                              }))
      | Pressed <- motion = EKeyDown keycode
      | Released <- motion = EKeyUp keycode
    convert _ = EUnknown

updatePlayer :: State -> Int -> (Player -> Player) -> State
updatePlayer w i f = w {players = IM.adjust f i (players w)}

step :: Int -> Float -> State -> State
step playerNum dt w@(State {..}) = w
  { worldTime = worldTime + dt
  , lastDts = Seq.take 100 (dt Seq.:<| lastDts)
  , players = IM.map (updatePos dt) players
  }
 where
  updatePos dt p@Player {..} = if walkable worldMap newPos
    then p { playerPos = newPos }
    else p
   where
    newPos = playerPos + dt *^ speed
    keyToDir k dir = if S.member k keysPressed then dir else V2 0 0
    speed =
      (if S.member KeycodeLShift keysPressed then 4 else 2)
        *^ normalize (keyToDir KeycodeW playerDir
           + keyToDir KeycodeS (-1 *^ playerDir)
           + keyToDir KeycodeA (perp playerDir)
           + keyToDir KeycodeD (-perp playerDir))

handle :: Int -> State -> SimpleEvent -> State
--handle playerNum event _ | traceShow (playerNum, event) False = undefined
handle playerNum w@(State {..}) event = handle' event
  where
    handle' (EMotion x _) =
      updatePlayer w playerNum $ \player ->
        player {playerDir = rotateV2 (-x / 300) (playerDir player)}
    handle' (EKeyDown k) =
      updatePlayer w playerNum $ \player ->
        player {keysPressed = S.insert k (keysPressed player)}
    handle' (EKeyUp k) =
      updatePlayer w playerNum $ \player ->
        player {keysPressed = S.delete k (keysPressed player)}
    handle' _ = w

{- RAY CASTING -}
data HitSide
  = HInside
  | HN
  | HS
  | HE
  | HW
  deriving (Show)

-- from http://www.cse.yorku.ca/~amana/research/grid.pdf
cellsVisitedByRay ::
     V2 Float -- starting point
  -> V2 Float -- direction
  -> [(HitSide, (Int, Int), Float)]
cellsVisitedByRay (V2 posX posY) (V2 dirX dirY) =
  (HInside, (initI, initJ), 0) : go initI initJ initTMaxX initTMaxY
  where
    initI = floor posX
    initJ = floor posY
    stepI =
      if dirX > 0
        then 1
        else -1
    stepJ =
      if dirY > 0
        then 1
        else -1
    tDeltaX = abs (1 / dirX)
    tDeltaY = abs (1 / dirY)
    xSide =
      if dirX > 0
        then HW
        else HE
    ySide =
      if dirY > 0
        then HS
        else HN
    initTMaxX =
      if dirX > 0
        then (1 + i2f initI - posX) * tDeltaX
        else (posX - i2f initI) * tDeltaX
    initTMaxY =
      if dirY > 0
        then (1 + i2f initJ - posY) * tDeltaY
        else (posY - i2f initJ) * tDeltaY
    go i j tMaxX tMaxY
      | tMaxX < tMaxY =
        let i' = i + stepI
            tMaxX' = tMaxX + tDeltaX
         in (xSide, (i', j), tMaxX) : go i' j tMaxX' tMaxY
      | otherwise =
        let j' = j + stepJ
            tMaxY' = tMaxY + tDeltaY
         in (ySide, (i, j'), tMaxY) : go i j' tMaxX tMaxY'

type Collision =
  ( HitSide
  , WallType
  , Float -- distance to camera plane
  , Float -- texture coordinate
  )

collision ::
     Map
  -> V2 Float -- starting point
  -> V2 Float -- direction of camera
  -> V2 Float -- direction of ray
  -> Collision
collision m pos camDir rayDir =
  head $ filter isWall $ map convert $ cellsVisitedByRay pos rayDir
  where
    convert (side, coord, d) =
      (side, m A.! coord, d * cos (angleBetween camDir rayDir), textureCoord side d)
    textureCoord HInside _ = 1
    textureCoord HN d = ((pos + d *^ rayDir) ^. _x) & fix
    textureCoord HS d = ((pos + d *^ rayDir) ^. _x) & fix
    textureCoord HE d = ((pos + d *^ rayDir) ^. _y) & fix
    textureCoord HW d = ((pos + d *^ rayDir) ^. _y) & fix
    fix x = if tx > 0 then tx else tx + 1
      where tx = x & properFraction & snd
    isWall (_, wallType, _, _) = wallType > 0

type Collisions = A.Array Int Collision

collisions :: Map -> V2 Float -> V2 Float -> Collisions
collisions m pos dir = A.listArray (-halfScreenWidth, halfScreenWidth) cols
  where
    cols =
      map (collision m pos dir . rayDir) [-halfScreenWidth .. halfScreenWidth]
    rayDir i = rotateV2 (-fov * i2f i / i2f screenWidth) dir

{- RENDERING -}
screenWidth :: Int
screenWidth = 640

screenHeight :: Int
screenHeight = 480

halfScreenWidth :: Int
halfScreenWidth = screenWidth `div` 2

halfScreenHeight :: Int
halfScreenHeight = screenHeight `div` 2

screenRatio :: Float
screenRatio = i2f screenWidth / i2f screenHeight

fov :: Float
fov = pi / 4

angleToX :: Float -> Float
angleToX a = ((-a) * i2f screenWidth) / fov

minimapColor :: Int -> Color
minimapColor 0 = D.white
minimapColor 1 = D.grey 0.5
minimapColor 2 = D.orange
minimapColor 3 = D.green
minimapColor 4 = D.orange
minimapColor n = error ("unknown wall type " ++ show n)

world :: Int -> Tex.Textures -> Tex.Textures -> Word32 -> State -> D.Picture
world playerNum wallTexs spriteTexs transCol State {..} =
  background <> walls worldTime wallTexs cols <> sprites spriteTexs transCol playerNum cols players
  where
    Player {..} = players IM.! playerNum
    cols = collisions worldMap playerPos playerDir

background = topHalf 0.8 <> D.scale (V2 1 (-1)) (topHalf 0.6)
  where
    topHalf brightness = D.translate (V2 0 (hh/2)) $ D.color (D.grey brightness) (D.rectangle w hh)
    w = i2f screenWidth
    hh = i2f halfScreenHeight

walls :: Float -> Tex.Textures -> Collisions -> D.Picture
walls time texs cols = mconcat (map wallSlice (A.assocs cols))
 where
  wallSlice (i, (hitSide, wallType, distance, textureCoord))
    |
    -- we should get rid of that case once we cannot cross closed doors
      distance == 0
    = mempty
    | otherwise
    = let x = i2f i
          h = i2f screenHeight / (distance * screenRatio)
          column = Tex.columnAt
            (Tex.textureAt texs (wallTexture hitSide wallType tick))
            (round (textureCoord * 63))
      in  D.vline column Nothing (V2 x h) (-2 * h)
  tick = floor (time * 15)

wallTexture :: HitSide -> Int -> Int -> Int
wallTexture HN 1 _ = 0
wallTexture HS 1 _ = 0
wallTexture HE 1 _ = 1
wallTexture HW 1 _ = 1
wallTexture HN 3 _ = 2
wallTexture HS 3 _ = 2
wallTexture HE 3 _ = 3
wallTexture HW 3 _ = 3
wallTexture HN 2 _ = 4
wallTexture HS 2 _ = 4
wallTexture HE 2 _ = 5
wallTexture HW 2 _ = 5
wallTexture _ 4 tick = 6 + tick `mod` 8
wallTexture _ _ _ = error "unknown wall type"

sprites :: Tex.Textures -> Word32 -> Int -> Collisions -> IM.IntMap Player -> D.Picture
sprites texs transCol playerNum cols players = mconcat (map sprite sortedOthers)
  where
    myPlayer = players IM.! playerNum
    cameraPos = playerPos myPlayer
    cameraDir = playerDir myPlayer
    otherPlayers = IM.delete playerNum players
    distanceToPlayer Player {..} = norm (playerPos - cameraPos)
    sortedOthers = sortBy (comparing (Down . distanceToPlayer)) (IM.elems otherPlayers)
    sprite Player {..} =
      let directionToOther = playerPos - cameraPos
          distanceToOther = norm directionToOther
          h = i2f screenHeight / (distanceToOther * screenRatio)
          center = round $ angleToX (angleBetween cameraDir directionToOther)
          width = round (2*h)
          halfWidth = width `div` 2
          minI = center - halfWidth
          maxI = center + halfWidth
          isVisible i =
            i > -halfScreenWidth &&
            i < halfScreenWidth &&
            let (_, _, d, _) = cols A.! i
             in d > distanceToOther
          slicesToDraw = filter isVisible [minI .. maxI]
          iToColumn i = (i - minI) * 63 `div` width
          angleBetweenPlayerAndOther = angleBetween cameraDir playerDir
          spriteIndex = (-round ((angleBetweenPlayerAndOther+pi) * 9 / (2*pi))) `mod` 8
          spriteSlice i =
            let column = Tex.columnAt (Tex.textureAt texs spriteIndex) (iToColumn i)
            in D.vline column (Just transCol) (V2 (i2f i) h) (-2 * h)
       in mconcat (map spriteSlice slicesToDraw)

hud :: Int -> State -> D.Picture
hud playerNum state =
  D.translate
    (V2
       (-i2f halfScreenWidth + margin)
       (i2f halfScreenHeight - minimapHeight - margin)) $
  D.scale (V2 scaleFactor scaleFactor) $ minimap playerNum state
  where
    minimapWidth = i2f screenWidth / 5
    scaleFactor = minimapWidth / i2f w
    margin = 1.5 * scaleFactor
    minimapHeight = i2f h * scaleFactor
    (_, (w, h)) = A.bounds (worldMap state)

minimap :: Int -> State -> D.Picture
minimap playerNum State {..} =
  mconcat [cell i j | i <- [0 .. w], j <- [0 .. h]] <>
  mconcat [player i p | (i, p) <- IM.toList players]
  where
    (_, (w, h)) = A.bounds worldMap
    cell i j =
      D.translate (V2 (i2f i) (i2f j)) $
      D.color (minimapColor (worldMap A.! (i, j))) $ D.rectangle 1 1
    player i Player {..} =
      D.translate playerPos $
      D.color
        (if i == playerNum
           then D.blue
           else D.red) $
      (D.thickPoint 2 <> D.line (V2 0 0) playerDir)

debugInfo font State{..} =
  D.translate (V2 (i2f $ 10 - halfScreenWidth) (i2f $ 20 - halfScreenHeight))
    $ D.color D.green
    $ D.text font (T.pack (show fps) <> " fps")
 where fps = round (i2f (length lastDts) / sum lastDts)

screen
  :: Int
  -> Tex.Textures
  -> Tex.Textures
  -> Word32
  -> Font.Font
  -> State
  -> D.Picture
screen playerNum wallTexs spriteTexs transCol font state = adjust
  (  world playerNum wallTexs spriteTexs transCol state
  <> hud playerNum state
  <> debugInfo font state
  )
 where
  adjust = D.translate (V2 (i2f halfScreenWidth) (i2f halfScreenHeight))
    . D.scale (V2 1 (-1))

render
  :: Int
  -> Tex.Textures
  -> Tex.Textures
  -> Word32
  -> Font.Font
  -> Surface
  -> State
  -> IO ()
render playerNum wallTexs spriteTexs transCol font surface state@State {..} =
  D.render surface (screen playerNum wallTexs spriteTexs transCol font state)

{- MAIN -}
initialState :: Int -> StdGen -> State
initialState n gen =
  State
    { worldMap = parseMap testMap
    , worldTime = 0
    , lastDts = Seq.empty
    , players =
        IM.fromList
          [ (i, Player pos dir S.empty)
          | (i, (pos, dir)) <-
              [0 .. n - 1] `zip` filter walkableInit randomInits
          ]
    }
  where
    worldMap = parseMap testMap
    (_, (w, h)) = A.bounds worldMap
    randomInits = toVectors (randomRs (0, 1) gen)
    toVectors (x:y:dx:dy:xs) = (toPos x y, toDir dx dy) : toVectors xs
    toPos x y = V2 (i2f w * x) (i2f h * y)
    toDir dx dy = normalize (V2 dx dy)
    walkableInit (pos, _) = walkable worldMap pos

windowConfig =
  defaultWindow
    { windowInitialSize =
        V2 (fromIntegral screenWidth) (fromIntegral screenHeight)
    -- , windowMode = Fullscreen
    }

main :: IO ()
main = do
  initializeAll
  Font.initialize
  window <- createWindow "My SDL Application" windowConfig
  Surface surfacePtr _ <- getWindowSurface window
  format <- Raw.surfaceFormat <$> peek surfacePtr
  transCol <- Raw.mapRGB format 153 0 137
  wallTextures <- Tex.loadTextures format 4 4 "data/walls-alt.png"
  spriteTextures <- Tex.loadTextures format 8 1 "data/sprites-alt.png"
  monoFont <- Font.load "/usr/share/fonts/truetype/freefont/FreeMono.ttf" 12
  setMouseLocationMode RelativeLocation
  stdGen <- getStdGen
  initTime <- time
  appLoop wallTextures spriteTextures transCol monoFont window (initialState 20 stdGen) initTime
  Font.quit

appLoop :: Tex.Textures -> Tex.Textures -> Word32 -> Font.Font -> Window -> State -> Float -> IO ()
appLoop wallTextures spriteTextures transCol font window state lastTime = do
  initTime <- time
  loop state lastTime
  where
    loop state lastTime = do
      events <- map eventToSimpleEvent <$> pollEvents
      let state' = foldl' (handle 0) state events
      currentTime <- time
      let state'' = step 0 (currentTime - lastTime) state'
      surface <- getWindowSurface window
      render 0 wallTextures spriteTextures transCol font surface state''
      updateWindowSurface window
      loop state'' currentTime
