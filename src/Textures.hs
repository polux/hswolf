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

module Textures where

import qualified SDL
import qualified SDL.Primitive as SDL
import qualified SDL.Raw as Raw
import qualified Codec.Picture as Pic
import qualified Codec.Picture.Extra as Pic
import qualified Data.Vector.Unboxed as UVec
import qualified Data.Vector as Vec
import Data.Coerce (coerce)
import Data.Function ((&))
import Foreign.Ptr (Ptr(..))
import Data.Word (Word32)
import Data.Functor ((<&>))

type Column = UVec.Vector Word32
newtype Texture = Texture (Vec.Vector Column)
newtype Textures = Textures (Vec.Vector Texture)
  deriving(Show)

instance Show Texture where
  show (Texture vec) = show (Vec.length vec) ++ "x" ++ show (UVec.length (vec Vec.! 0))

textureAt :: Textures -> Int -> Texture
textureAt texs i = coerce texs Vec.! i

columnAt :: Texture -> Int -> Column
columnAt tex i = coerce tex Vec.! i

loadTextures :: Ptr Raw.PixelFormat -> Int -> Int -> FilePath -> IO Textures
loadTextures format numCols numRows path = do
  errorOrDynImg <- Pic.readPng path
  case errorOrDynImg of
    Left err -> error err
    Right dynImg ->
        dynImg
        & Pic.convertRGB8
        & extractSquares 64 numCols numRows
        & Vec.mapM (imageToTexture format)
        <&> Textures

type Img = Pic.Image Pic.PixelRGB8

imageToTexture :: Ptr Raw.PixelFormat -> Img -> IO Texture
imageToTexture format img = Texture <$> Vec.generateM width generateColumn
  where
    width = Pic.imageWidth img
    height = Pic.imageHeight img
    generateColumn i = UVec.generateM height (pixelToColor . pixelAt i)
    pixelAt i j = Pic.pixelAt img i j
    pixelToColor (Pic.PixelRGB8 r g b) = Raw.mapRGB format r g b

extractSquares :: Int -> Int -> Int -> Img -> Vec.Vector Img
extractSquares squareWidth numCols numRows img = Vec.fromList
  [ extractSquare i j
  | j <- [0..numRows-1]
  , i <- [0..numCols-1]
  ]
 where
  extractSquare i j = Pic.crop (i*squareWidth) (j*squareWidth) squareWidth squareWidth img
