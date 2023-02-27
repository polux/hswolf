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
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}

module Draw
  ( Picture()
  , line
  , rectangle
  , vline
  , thickPoint
  , rotate
  , translate
  , scale
  , color
  , white
  , black
  , grey
  , red
  , green
  , blue
  , orange
  , darken
  , render
  , text
  ) where

import Control.Monad (when, forM_)
import qualified Data.Vector.Unboxed as UVec
import qualified Data.Vector.Storable as SVec
import Debug.Trace
import Lens.Micro ((^.))
import Linear (M33(..), V2(..), V3(..), V4(..), (!*), (!*!), identity)
import SDL (($=))
import qualified SDL
import qualified SDL.Primitive as SDL
import qualified SDL.Font as Font
import qualified Textures as Tex
import Foreign.Ptr (Ptr, castPtr, plusPtr)
import Data.Word (Word32)
import Foreign.Storable (poke)
import Foreign.C.Types (CInt)
import qualified Data.Text as T

{- PUBLIC -}
white :: SDL.Color
white = V4 255 255 255 255

black :: SDL.Color
black = V4 0 0 0 255

grey :: S -> SDL.Color
grey s = V4 n n n 255
  where
    n = round (s * 255)

red :: SDL.Color
red = V4 255 0 0 255

green :: SDL.Color
green = V4 0 255 0 255

blue :: SDL.Color
blue = V4 0 0 255 255

orange :: SDL.Color
orange = V4 255 165 0 255

darken :: Float -> SDL.Color -> SDL.Color
darken s (V4 r g b a) = V4 (dim r) (dim g) (dim b) a
  where
    dim n = round (s * fromIntegral n)

line :: V -> V -> Picture
line (V2 x1 y1) (V2 x2 y2) = Line (V3 x1 y1 1) (V3 x2 y2 1)

rectangle :: S -> S -> Picture
rectangle = FillRectangle

vline :: Tex.Column -> Maybe Word32 -> V -> S -> Picture
vline column transCol (V2 x y) = VLine column transCol (V3 x y 1)

thickPoint :: S -> Picture
thickPoint = ThickPoint

text :: Font.Font -> T.Text -> Picture
text = Text

rotate :: S -> Picture -> Picture
rotate = transform . rotateM

translate :: V -> Picture -> Picture
translate = transform . translateM

scale :: V -> Picture -> Picture
scale = transform . scaleM

color :: SDL.Color -> Picture -> Picture
color = Color

instance Semigroup Picture where
  (<>) = Superpose

instance Monoid Picture where
  mempty = Empty

origin :: HP
origin = V3 0 0 1


render :: SDL.Surface -> Picture -> IO ()
render surface pic = do
  bounds <- SDL.surfaceDimensions surface
  pixels <- castPtr <$> SDL.surfacePixels surface
  renderer <- SDL.createSoftwareRenderer surface
  draw (fmap fromIntegral bounds) surface pixels renderer pic


draw :: V2 Int -> SDL.Surface -> Ptr Word32 -> SDL.Renderer -> Picture -> IO ()
draw bounds surface pixels renderer pic = draw' white identity pic
  where
    draw' :: SDL.Color -> TM -> Picture -> IO ()
    draw' col m Empty = return ()
    -- We don't need to optimize for horizontal and vertical lines: SDL gfx does it already,
    -- see http://www.ferzkopp.net/Software/SDL_gfx-2.0/Docs/html/_s_d_l__gfx_primitives_8c_source.html#l02589
    draw' col m (Line p1 p2) =
      SDL.smoothLine renderer (pin m p1) (pin m p2) col
    draw' col m (FillRectangle w h)
      | noRotation m = renderFillRectangle m col w h
      | otherwise = renderFillPolygon m col (rectangleToVertices w h)
    draw' col m (FillPolygon ps) = renderFillPolygon m col ps
    draw' col m (VLine column transparentColor p@(V3 x y z) h)
      | noRotation m = drawColumn pixels bounds column transparentColor i j1 j2
      | otherwise = error "cannot rotate vline"
      where
        (V2 i j1) = pin m p
        (V2 _ j2) = pin m (p + V3 0 h 0)
    draw' col m (ThickPoint r) = do
      SDL.fillCircle renderer center ir col
      SDL.smoothCircle renderer center ir col
      where
        center = pin m origin
        ir = round r
    draw' col m (Text font str) = do
      renderedText <- Font.solid font col str
      _ <- SDL.surfaceBlit renderedText Nothing surface (Just (SDL.P (pin m (V3 0 0 1))))
      SDL.freeSurface renderedText
    draw' col m (Superpose p1 p2) = do
      draw' col m p1
      draw' col m p2
    draw' col m1 (Transform m2 p) = draw' col (m1 !*! m2) p
    draw' _ m (Color col p) = draw' col m p
    noRotation (V3 (V3 _ 0 _) (V3 0 _ _) (V3 0 0 _)) = True
    noRotation _ = False
    pin :: Integral a => TM -> HP -> V2 a
    pin m p = toPos (m !* p)
    rectangleToVertices :: S -> S -> [HP]
    rectangleToVertices w h =
      [ V3 (-w / 2) (-h / 2) 1
      , V3 (w / 2) (-h / 2) 1
      , V3 (w / 2) (h / 2) 1
      , V3 (-w / 2) (h / 2) 1
      ]
    renderFillRectangle :: TM -> SDL.Color -> S -> S -> IO ()
    renderFillRectangle m col w h = SDL.fillRectangle renderer p1 p2 col
      where
        p1 = pin m (V3 (-w / 2) (-h / 2) 1)
        p2 = pin m (V3 (w / 2) (h / 2) 1)
    renderFillPolygon :: TM -> SDL.Color -> [HP] -> IO ()
    renderFillPolygon m col ps = do
      SDL.fillPolygon renderer xsv ysv col
      SDL.smoothPolygon renderer xsv ysv col
      where
        xsv = SVec.fromList xs
        ysv = SVec.fromList ys
        (xs, ys) = unzip (map (toPair . pin m) ps)
        toPair (V2 x y) = (x, y)

drawColumn
  :: Ptr Word32
  -> V2 Int
  -> Tex.Column
  -> Maybe Word32
  -> Int
  -> Int
  -> Int
  -> IO ()
drawColumn pixels (V2 w h) column transparentColor i j1 j2
  | i >= 0 && i < w = case transparentColor of
    Just transCol -> js `forM_` \j -> do
      let txl = texel j
      when (txl /= transCol) (poke (plusPtr pixels (offset j)) txl)
    Nothing -> js `forM_` \j -> poke (plusPtr pixels (offset j)) (texel j)
  | otherwise = return ()
 where
  jmin = max 0 (min j1 j2)
  jmax = min (h - 1) (max j1 j2)
  js   = [jmin, jmin + 1 .. jmax]
  len  = j2 - j1
  tlen = UVec.length column
  texel j = column UVec.! ((j - j1) * (tlen - 1) `div` len)
  offset j = (w * j + i) * 4

{- PRIVATE -}
-- scalars
type S = Float

-- 2d vector
type V = V2 S

-- point in homogenous coordinates
type HP = V3 S

-- transformation matrix
type TM = M33 S

data Picture
  = Empty
  | Line HP HP
  | FillRectangle S S
  | FillPolygon [HP]
  | VLine Tex.Column (Maybe Word32) HP S
  | ThickPoint S
  | Text Font.Font T.Text
  | Superpose Picture Picture
  | Color SDL.Color Picture
  | Transform TM Picture
  deriving (Show)

toPos :: Integral a => HP -> V2 a
toPos (V3 x y z) = V2 (round x) (round y)

translateM :: V -> TM
translateM (V2 tx ty) = V3 (V3 1 0 tx) (V3 0 1 ty) (V3 0 0 1)

rotateM :: S -> TM
rotateM angle =
  V3 (V3 (cos angle) (-sin angle) 0) (V3 (sin angle) (cos angle) 0) (V3 0 0 1)

scaleM :: V -> TM
scaleM (V2 sx sy) = V3 (V3 sx 0 0) (V3 0 sy 0) (V3 0 0 1)

transform :: TM -> Picture -> Picture
transform m1 (Transform m2 p) = Transform (m1 !*! m2) p
transform m p = Transform m p
