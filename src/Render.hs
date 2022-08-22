module Render where

import qualified SDL
import qualified SDL.Image
import qualified Data.HashMap.Strict as HM
import Foreign.C.Types
import Control.Monad (when, forM, forM_)
import Data.StateVar
import Data.Word
import Data.Foldable

import Common
import Zertz
import ZertzLogic

data FSCP = FieldPart | Selected | Chosen | Possible

scale :: CFloat
scale = 0.2

token_field_scale :: CFloat
token_field_scale = 0.7

token_scale :: CFloat
token_scale = 0.9

margin :: CInt
margin = 3

margin_pl = 3

data ZertzRenderInfo = ZertzRenderInfo {
  tokensTex      :: SDL.Texture,
  tokensTexSize  :: CInt,
  fieldTex      :: SDL.Texture,
  fieldTexSize  :: CInt,
  playersTex       :: SDL.Texture,
  playersTexHeigth :: CInt,
  playersTexWidth  :: CInt,
  endTex       :: SDL.Texture,

  -- info about sizes on screen
  size_field :: CInt,
  size_freetoken :: CInt,
  size_fieldtoken :: CInt,
  
  player1Rect :: SDL.Rectangle CInt,
  player2Rect :: SDL.Rectangle CInt,

  --starting points for drawing data
  initFree  :: (CInt, CInt),
  initPl1   :: (CInt, CInt),
  initPl2   :: (CInt, CInt),
  initField :: (CInt, CInt)
}

token_tpath :: FilePath
token_tpath = "./textures/tokens.png"

field_tpath :: FilePath
field_tpath = "./textures/field2.png"

players_tpath :: FilePath
players_tpath = "./textures/players.png"

end_tpath :: FilePath
end_tpath = "./textures/end.png"

loadInfo :: SDL.Window -> SDL.Renderer-> IO ZertzRenderInfo
loadInfo w r = do
  (SDL.V2 width heigth) <- get $ SDL.windowSize w
  (token, (SDL.TextureInfo _ _ _ tok_h)) <- loadTextureWithInfo r token_tpath 
  (field, (SDL.TextureInfo _ _ _ field_h)) <- loadTextureWithInfo r field_tpath 
  (players, (SDL.TextureInfo _ _ p_w p_h)) <- loadTextureWithInfo r players_tpath 
  (end, _) <- loadTextureWithInfo r end_tpath 

  let s_field  = floor $ (0.5 - scale) / (fromInteger fieldRad * sqrt 3.0 + 1.0) * fromIntegral heigth
      s_token  = floor $ token_field_scale * fromIntegral s_field
      s_free   = floor $ (scale * fromIntegral heigth) / 6.0

      p_width  = floor $ scale * fromIntegral heigth
      p_height = floor $ fromIntegral p_h  * (scale * fromIntegral heigth) / fromIntegral p_w

      player1rect = mkRect margin (heigth - 1 - p_height - margin) p_width p_height
      player2rect = mkRect (width - 1 - p_width - margin) (heigth - 1 - p_height - margin) p_width p_height

      st_field = (width `div` 2, heigth `div` 2)
      st_free  = (margin + s_free, margin + s_free)
      st_p1    = (margin + s_free,                       heigth - 1 - p_height - margin - s_free - margin_pl)
      st_p2    = (width - 1 - p_width + s_free - margin, heigth - 1 - p_height - margin - s_free - margin_pl)

  SDL.textureBlendMode token $= SDL.BlendAlphaBlend
  SDL.textureBlendMode field $= SDL.BlendAlphaBlend
  SDL.textureBlendMode players $= SDL.BlendAlphaBlend
  SDL.textureBlendMode end $= SDL.BlendAlphaBlend

  return $ ZertzRenderInfo {
    tokensTex      = token,
    tokensTexSize  = tok_h,
    fieldTex       = field,
    fieldTexSize   = field_h,
    playersTex       = players,
    playersTexHeigth = p_h `div` 2,
    playersTexWidth  = p_w,
    endTex         = end,

    size_field     = s_field,
    size_freetoken = s_free,
    size_fieldtoken = s_token,
    
    player1Rect = player1rect,
    player2Rect = player2rect,

    --starting points for drawing data
    initFree  = st_free,
    initPl1   = st_p1,
    initPl2   = st_p2,
    initField = st_field
  }

deleteTextures :: ZertzRenderInfo -> IO ()
deleteTextures zri = do 
  SDL.destroyTexture $ tokensTex zri
  SDL.destroyTexture $ fieldTex zri
  SDL.destroyTexture $ playersTex zri
  SDL.destroyTexture $ endTex zri

-- render
render :: SDL.Renderer -> ZertzRenderInfo -> Zertz -> IO ()
render r t w = do
  SDL.clear r
  drawZertz r t w
  SDL.present r

drawZertz :: SDL.Renderer -> ZertzRenderInfo -> Zertz -> IO ()
drawZertz r t z = do
  clearWithActive r z
  drawField r t z
  drawPossible r t z 
  drawChosen r t z 
  drawSelected r t z
  drawFree r t z
  drawPlayers r t
  drawPlayersTokens r t z

  if _gamePhase z == EndOfTheGame 
    then drawEnd r t
    else return ()

  SDL.present r

drawEnd :: SDL.Renderer -> ZertzRenderInfo -> IO ()
drawEnd r zri = SDL.copy r (endTex zri) Nothing Nothing


drawField :: SDL.Renderer -> ZertzRenderInfo -> Zertz -> IO ()
drawField r zri z = do 
  sequence_ $ map f (HM.toList $ _field z)

    where 
      f (c, Nothing) = drawOneField r zri FieldPart c'
        where
          c' = getCenterField zri c
      f (c, Just t)  = drawOneField r zri FieldPart c' >> drawOneFieldToken r zri t c'
        where
          c' = getCenterField zri c


drawChosen :: SDL.Renderer -> ZertzRenderInfo -> Zertz -> IO ()
drawChosen r zri z = do
  let c = _activeJump z

  case c of 
    Nothing -> return ()
    Just c' -> do let cent = getCenterField zri c'
                  drawOneField r zri Chosen cent

drawPossible :: SDL.Renderer -> ZertzRenderInfo -> Zertz -> IO ()
drawPossible r zri z = do
  let cs = _possibleSelection z
      cents = map (getCenterField zri) cs

  sequence_ $ map (drawOneField r zri Possible) cents

drawSelected :: SDL.Renderer -> ZertzRenderInfo -> Zertz -> IO ()
drawSelected r zri z = do 
  let c = _selected z
      cent = getCenterField zri c

  drawOneField r zri Selected cent

drawPlayersTokens :: SDL.Renderer -> ZertzRenderInfo -> Zertz -> IO ()
drawPlayersTokens r zri z = do
  let [n11, n21, n31] = map (_playersTokens z HM.! P1 HM.!) [T1, T2, T3]
      [n12, n22, n32] = map (_playersTokens z HM.! P2 HM.!) [T1, T2, T3]

  sequence_ $ map (drawOneFreeToken r zri T1 . getCenterPlayer P1 T1 zri) [0..(n11-1)]
  sequence_ $ map (drawOneFreeToken r zri T2 . getCenterPlayer P1 T2 zri) [0..(n21-1)]
  sequence_ $ map (drawOneFreeToken r zri T3 . getCenterPlayer P1 T3 zri) [0..(n31-1)]

  sequence_ $ map (drawOneFreeToken r zri T1 . getCenterPlayer P2 T1 zri) [0..(n12-1)]
  sequence_ $ map (drawOneFreeToken r zri T2 . getCenterPlayer P2 T2 zri) [0..(n22-1)]
  sequence_ $ map (drawOneFreeToken r zri T3 . getCenterPlayer P2 T3 zri) [0..(n32-1)]


drawFree :: SDL.Renderer -> ZertzRenderInfo -> Zertz -> IO ()
drawFree r zri z = do 
  let [n1, n2, n3] = map (_freeTokens z HM.!) [T1, T2, T3]
  
  sequence_ $ map (drawOneFreeToken r zri T1 . getCenterFree T1 zri) [0..(n1-1)]
  sequence_ $ map (drawOneFreeToken r zri T2 . getCenterFree T2 zri) [0..(n2-1)]
  sequence_ $ map (drawOneFreeToken r zri T3 . getCenterFree T3 zri) [0..(n3-1)]


drawOneFreeToken :: SDL.Renderer -> ZertzRenderInfo -> Token -> (CInt, CInt) -> IO ()
drawOneFreeToken r zri t (x, y) = do 
  let rectFrom = mkRect (nx * sizeFrom) 0 sizeFrom sizeFrom
      rectTo   = mkRect (x - sizeTo) (y - sizeTo) (2*sizeTo) (2*sizeTo)

  SDL.copy r (tokensTex zri) (Just rectFrom) (Just rectTo)

    where
      nx = case t of
        T1 -> 0
        T2 -> 1
        T3 -> 2

      sizeFrom = tokensTexSize zri
      sizeTo   = floor $ token_scale * fromIntegral (size_freetoken zri) 

drawPlayers :: SDL.Renderer -> ZertzRenderInfo -> IO ()
drawPlayers r zri = do 
  SDL.copy r (playersTex zri) (Just rectFrom1) (Just rectTo1)
  SDL.copy r (playersTex zri) (Just rectFrom2) (Just rectTo2)
    where
      hsize = playersTexHeigth zri
      wsize = playersTexWidth zri

      rectFrom1 = mkRect 0 0 wsize hsize
      rectFrom2 = mkRect 0 hsize wsize hsize

      rectTo1 = player1Rect zri 
      rectTo2 = player2Rect zri

drawOneField :: SDL.Renderer -> ZertzRenderInfo -> FSCP -> (CInt, CInt) -> IO ()
drawOneField r zri fscp (x, y) = do 
  let rectFrom = mkRect (sizeFrom * n) 0 sizeFrom sizeFrom
      rectTo   = mkRect (x - sizeTo) (y - sizeTo) (2*sizeTo) (2*sizeTo)

  SDL.copy r (fieldTex zri) (Just rectFrom) (Just rectTo)

  where
    sizeFrom = fieldTexSize zri
    sizeTo   = size_field zri

    n = case fscp of
      FieldPart -> 0
      Selected -> 3
      Chosen -> 1
      Possible -> 2

drawOneFieldToken :: SDL.Renderer -> ZertzRenderInfo -> Token -> (CInt, CInt) -> IO ()
drawOneFieldToken r zri t (x, y) = do 
  let rectFrom = mkRect (nx * sizeFrom) 0 sizeFrom sizeFrom
      rectTo   = mkRect (x - sizeTo) (y - sizeTo) (2*sizeTo) (2*sizeTo)

  SDL.copy r (tokensTex zri) (Just rectFrom) (Just rectTo)

    where
      nx = case t of
        T1 -> 0
        T2 -> 1
        T3 -> 2

      sizeFrom = tokensTexSize zri
      sizeTo   = size_fieldtoken zri


getCenterField :: ZertzRenderInfo -> Coord -> (CInt, CInt)
getCenterField zri (x,y) = let (cx, cy) = initField zri; r = size_field zri in 
  (cx +  r * fromInteger (2 * x - y), cy + floor (sqrt 3 * fromIntegral (r * fromInteger y)))

getCenterFree :: Token -> ZertzRenderInfo -> Integer -> (CInt, CInt)
getCenterFree t zri n = let (cx, cy) = initFree zri; r = size_freetoken zri in 
  (cx + fromInteger n * r * 2, cy + y * 2 * r)
  where 
    y = case t of 
      T1 -> 0 :: CInt
      T2 -> 1 :: CInt
      T3 -> 2 :: CInt

getCenterPlayer :: Player -> Token ->  ZertzRenderInfo -> Integer -> (CInt, CInt)
getCenterPlayer pl t zri n = let (cx, cy) = f zri; r = size_freetoken zri in 
  (cx + x * r * 2, cy - fromInteger n * 2 * r)
  where 
    x = case t of 
      T1 -> 0 :: CInt
      T2 -> 1 :: CInt
      T3 -> 2 :: CInt

    f = case pl of
      P1 -> initPl1
      P2 -> initPl2

clearWithActive :: SDL.Renderer -> Zertz -> IO ()
clearWithActive r z = do
  let pl = _activePlayer z
  case pl of 
    P1 -> SDL.rendererDrawColor r $= SDL.V4 0 0 64 255
    P2 -> SDL.rendererDrawColor r $= SDL.V4 64 0 0 255
  SDL.clear r





-- eventHandler

payloadToIntent :: SDL.EventPayload -> Intent
payloadToIntent SDL.QuitEvent         = Quit
payloadToIntent (SDL.KeyboardEvent kev)   = keyEventToIntent kev
payloadToIntent _                     = Idle

keyEventToIntent :: SDL.KeyboardEventData -> Intent
keyEventToIntent (SDL.KeyboardEventData _ SDL.Pressed _ keysym) =
  case SDL.keysymKeycode keysym of
    SDL.KeycodeEscape -> Quit

    SDL.KeycodeLeft  -> MvSelected RLeft
    SDL.KeycodeRight -> MvSelected RRight
    SDL.KeycodeDown  -> MvSelected RDown
    SDL.KeycodeUp    -> MvSelected RUp

    SDL.KeycodeReturn -> ClickSelected

    SDL.Keycode1     -> PlaceTk T1
    SDL.Keycode2     -> PlaceTk T2
    SDL.Keycode3     -> PlaceTk T3
    _                -> Idle

keyEventToIntent _ = Idle


-- updateZertz :: [SDL.Event] -> Zertz -> Zertz
-- updateZertz [] = id
-- updateZertz (i:is) = applyIntent (payloadToIntent $ SDL.eventPayload i) . updateZertz is

updateZertz :: Zertz -> [SDL.Event] -> Zertz
updateZertz z
  = foldl' (flip applyIntent) z
  . fmap (payloadToIntent . SDL.eventPayload)


