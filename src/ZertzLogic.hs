{-# LANGUAGE TemplateHaskell, BangPatterns #-}

module ZertzLogic where

import Zertz

import Control.Lens
import qualified Data.HashMap.Strict as HM
import Data.NumInstances.Tuple
import Data.List


data Intent = MvSelected RecDir 
            | PlaceTk Token
            | ClickSelected
            | Quit
            | Idle


-- the very main function
applyIntent :: Intent -> Zertz -> Zertz
applyIntent !i !z = case mz of
  Nothing -> z
  Just z' -> case next_state of 
    EndTurn      -> makeEndTurn z'
    CanJumpAgain -> if canActiveJumpAgain z' then changePhase Jump z' else makeEndTurn z'
    RemoveField  -> if (filter (flip isMovable z') . emptyCells $ z') /= [] 
                    then changePhase next_state z' 
                    else makeEndTurn z'

    _            -> changePhase next_state z'

  where 
    changePhase :: GamePhase -> Zertz -> Zertz
    changePhase ph' = set gamePhase ph'

    ph = _gamePhase z
    (eval, next_state) = evaluateGame i ph

    mz = eval . calculatePossibleSelections ph $ z



calculatePossibleSelections :: GamePhase -> Zertz -> Zertz
calculatePossibleSelections PlaceToken      z = z {_possibleSelection = emptyCells z}
calculatePossibleSelections RemoveField     z = z {_possibleSelection = filter (flip isMovable z) . emptyCells $ z}
calculatePossibleSelections ChooseJumpToken z = z {_possibleSelection = filter (flip canTokenJump z) . nonEmptyCells $ z}
calculatePossibleSelections Jump z            = z {_possibleSelection = map (translate2 start) posDirs}
  where
    fromJust (Just a) = a
    fromJust Nothing  = error "Impossible situation: gamePhase is Jump, but no active Jump cell has been set"

    start = fromJust . _activeJump $ z
    posDirs = filter (\d -> canTokenJumpDir start d z) dirlist

calculatePossibleSelections CanJumpAgain z = calculatePossibleSelections Jump z
calculatePossibleSelections _ z = z {_possibleSelection = []}



-- automata of game
evaluateGame :: Intent -> GamePhase -> (Zertz -> Maybe Zertz, GamePhase)
evaluateGame Quit EndOfTheGame = (Just, Stop)
evaluateGame _ EndOfTheGame    = (Just, EndOfTheGame)
evaluateGame (MvSelected d) x = (Just . moveSelected d, x)

evaluateGame (PlaceTk t)   PlaceToken      = (tryPlaceToken t,    RemoveField )

evaluateGame ClickSelected RemoveField     = (tryRemoveField,     EndTurn     )
evaluateGame ClickSelected ChooseJumpToken = (tryChooseJumpToken, Jump        )
evaluateGame ClickSelected Jump            = (tryJump,            CanJumpAgain)

evaluateGame Quit x = (Just, Stop)
evaluateGame _ x = (Just, x)



tryPlaceToken :: Token -> Zertz -> Maybe Zertz
tryPlaceToken t z | isPlaceMoveNotPossible z = error "Impossible situation"

                  | isFreeToken t z && isEmpty c z = 
      Just . (placeTokenToField t c) . (takeFreeToken t) $ z

                  | isPlayersToken t p z && not (isAtLeastOneFreeToken z) && isEmpty c z =
      Just . (placeTokenToField t c) . (takePlayersToken t p) $ z

                  | otherwise = Nothing
  where 
    c = z ^. selected
    p = z ^. activePlayer


tryRemoveField :: Zertz -> Maybe Zertz
tryRemoveField z = if c `elem` z ^. possibleSelection
      then Just (removeField c z) 
      else Nothing
  where c = z ^. selected


tryChooseJumpToken :: Zertz -> Maybe Zertz
tryChooseJumpToken z = if c `elem` z ^. possibleSelection 
    then Just $ activeJump .~ Just c $ z
    else Nothing
  where c = z ^. selected

tryJump :: Zertz -> Maybe Zertz
tryJump z = if sel `elem` z ^. possibleSelection 
  then  Just $ foldl (.) id [set activeJump $ Just $ _selected z,
                             placeTokenToField t sel, 
                             giveTokenToPlayer (z ^. activePlayer) tmid, 
                             removeTokenFromField mid,
                             removeTokenFromField start] $ z
  else Nothing
  where 
    start = fromJust $ _activeJump z
    sel   = z ^. selected

    mid = let (x,y) = start + sel in (x `div` 2, y `div` 2)

    t = fromJust $ (z ^. field) HM.! start 
    tmid = fromJust $ (z ^. field) HM.! mid

    fromJust (Just a) = a
    fromJust Nothing  = error "Impossible situation: try to jump from empty point"


-- verifications
canTokenJumpDir :: Coord -> Dir -> Zertz -> Bool
canTokenJumpDir c d z = (and . map ($ z)) [isNotEmpty c, isEmpty (translate2 c d), isNotEmpty (translate1 c d)]

canTokenJump :: Coord -> Zertz -> Bool
canTokenJump c z = or . map (\d -> canTokenJumpDir c d z) $ dirlist

-- properties of current configuration
isEmpty :: Coord -> Zertz -> Bool
isEmpty c z = isCell c z && ((z ^. field) HM.! c == Nothing)


isNotEmpty :: Coord -> Zertz -> Bool
isNotEmpty c z = isCell c z && ((z ^. field) HM.! c /= Nothing)

isCell :: Coord -> Zertz -> Bool
isCell c = HM.member c . (^. field)

isMovable :: Coord -> Zertz -> Bool
isMovable c z = case getNeigbExistingCoords c z of
  []                        -> True
  [_]                       -> True
  [_,_]                     -> True
  l@[_,_,_]                 -> sum l /= c+c+c
  l@[_,_,_,_]               -> (not . containsTriangle) l
  _                         -> False
  where 
    containsTriangle [c1,c2,c3,c4] = 
      c1 + c2 + c3 - c - c- c == (0, 0) ||
      c1 + c2 + c4 - c - c- c == (0, 0) ||
      c1 + c3 + c4 - c - c- c == (0, 0) ||
      c2 + c3 + c4 - c - c- c == (0, 0)



-- info about tokens
isAtLeastOneFreeToken :: Zertz -> Bool
isAtLeastOneFreeToken z = or $ flip isFreeToken z <$> [T1, T2, T3]

isFreeToken :: Token -> Zertz -> Bool
isFreeToken t z = (z ^. freeTokens) HM.! t > 0

isPlayersToken :: Token -> Player -> Zertz -> Bool
isPlayersToken t p z = (z ^. playersTokens) HM.! p HM.! t > 0

isPlaceMoveNotPossible :: Zertz -> Bool
isPlaceMoveNotPossible z = and . map noToken $ [z ^. freeTokens, (z ^. playersTokens) HM.! (z ^. activePlayer)]

isAnyJump :: Zertz -> Bool
isAnyJump z = any (flip canTokenJump z) . HM.keys . _field $ z

canActiveJumpAgain :: Zertz -> Bool
canActiveJumpAgain z = case _activeJump z of
  Nothing -> False
  Just c  -> canTokenJump c z

-- very basic actions
takeFreeToken :: Token -> Zertz -> Zertz
takeFreeToken t = freeTokens %~ HM.adjust (-1+) t

takePlayersToken :: Token -> Player -> Zertz -> Zertz
takePlayersToken t p = playersTokens %~ HM.adjust (HM.adjust (-1+) t) p 

giveTokenToPlayer :: Player -> Token -> Zertz -> Zertz
giveTokenToPlayer p t = playersTokens %~ HM.adjust (HM.insertWith (+) t 1) p 

placeTokenToField :: Token -> Coord -> Zertz -> Zertz
placeTokenToField t c = field %~ HM.insert c (Just t)

giveTokenToPlayerFromField :: Coord -> Zertz -> Zertz
giveTokenToPlayerFromField c z = case ((z ^. field) HM.! c) of
  Nothing -> z
  Just t  -> giveTokenToPlayer (z ^. activePlayer) t z

removeTokenFromField :: Coord -> Zertz -> Zertz
removeTokenFromField c = field %~ HM.insert c Nothing

removeField :: Coord -> Zertz -> Zertz
removeField c = field %~ HM.delete c


-- untils
getNeigbExistingCoords ::  Coord -> Zertz -> [Coord]
getNeigbExistingCoords c z = fmap (translate1 c) dirlist `intersect` HM.keys (z ^. field)


noToken :: TokenData -> Bool
noToken = (== [0,0,0]) . HM.elems

-- selection control
moveSelected :: RecDir -> Zertz -> Zertz
moveSelected rd z = z & selected %~ (+pre_offset) 
  where
    pre_offset = case rd of
      RRight -> ( 1, 0)
      RLeft  -> (-1, 0)
      RDown  -> if cond then ( 0,  1) else (1,  1)
      RUp    -> if cond then (-1, -1) else (0, -1)

    cond = even . snd . (_selected) $ z



emptyCells :: Zertz -> [Coord]
emptyCells = HM.keys . HM.filter (== Nothing) . _field

nonEmptyCells :: Zertz -> [Coord]
nonEmptyCells = HM.keys . HM.filter (/= Nothing) . _field

--

determineComponents :: [Coord] -> [[Coord]]
determineComponents l = determineComponents' l l
  where
    determineComponents' arr [] = []
    determineComponents' arr l@(c : l') = join comp neighbours
      where
        comp = determineComponents' arr l'
        neighbours = c : (translate1 c <$> dirlist) `intersect` arr

        join :: [[Coord]] -> [Coord] -> [[Coord]]
        join [] l = [l]
        join (comp:rest) l = if comp `intersect` l == [] 
          then comp : join rest l
          else join rest (comp `union` l)


determineFullComponents :: ZertzField -> [Coord]
determineFullComponents zf = foldl union [] . fmap (\c -> if (c `intersect` fulls) == c then c else []) $ comps
  where 
    fulls = HM.keys $ HM.filter (/=Nothing) zf
    comps = determineComponents . HM.keys $ zf


giveComponents :: Zertz -> Zertz
giveComponents z = foldl (.) id (fmap removeField fc ++ fmap giveTokenToPlayerFromField fc) $ z
  where
    fc = determineFullComponents $ _field z



makeEndTurn :: Zertz -> Zertz
makeEndTurn z = if isWinner (_playersTokens z' HM.! _activePlayer z')
    then set gamePhase EndOfTheGame z' 
    else set activeJump Nothing . changePlayer . (set gamePhase $ if isAnyJump z' then ChooseJumpToken else PlaceToken) $ z'
  where 
    z' = giveComponents z
    changePlayer :: Zertz -> Zertz
    changePlayer z'' = case _activePlayer z of
      P1  -> set activePlayer P2 $ z''
      P2  -> set activePlayer P1 $ z''


isStop :: Zertz -> Bool
isStop = (==Stop) . _gamePhase