{-# LANGUAGE OverloadedStrings #-}

module Main where

import Zertz
import ZertzLogic
import Render
import qualified Common as C

import Control.Monad.Loops    (iterateUntilM)
import qualified SDL
import qualified SDL.Image
import SDL                 (($=))

import Data.StateVar


main :: IO ()
main = C.withSDL $ C.withSDLImage $ do
  C.setHintQuality
  C.withWindow "Zertz" $ \w ->
    C.withRenderer w $ \r -> do
      SDL.rendererDrawBlendMode r $= SDL.BlendAlphaBlend

      zri <- loadInfo w r
      let doRender = render r zri

      _ <- iterateUntilM isStop (doProcess doRender) initialZertz

      
      deleteTextures zri



doProcess :: (Zertz -> IO ()) -> Zertz -> IO Zertz
doProcess doRender z  = do
  events <- SDL.pollEvents
  --if (events /= []) then print $ _possibleSelection z else return ()
  let z' = updateZertz z events 
  z' <$ doRender z'
