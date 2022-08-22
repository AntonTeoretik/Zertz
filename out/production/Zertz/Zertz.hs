{-# LANGUAGE TemplateHaskell, TupleSections #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Zertz where

import qualified Data.HashMap.Strict as HM
import Control.Lens
import Control.Lens.TH
import Data.Hashable
import GHC.Generics
import Data.NumInstances.Tuple



data Dir = L | R | DL | DR | UL | UR

dirlist :: [Dir]
dirlist = [L , R , DL , DR , UL , UR]


data RecDir = RUp | RDown | RLeft | RRight

type Coord = (Integer, Integer)


translate :: Integer -> Coord -> Dir -> Coord
translate n (x, y) L  = (x - n, y)
translate n (x, y) R  = (x + n, y)
translate n (x, y) DL = (x - n, y - n)
translate n (x, y) DR = (x, y - n)
translate n (x, y) UL = (x, y + n)
translate n (x, y) UR = (x + n, y + n)

translate1 :: Coord -> Dir -> Coord
translate1 = translate 1

translate2 :: Coord -> Dir -> Coord
translate2 = translate 2


data Token = T1 | T2 | T3
  deriving (Eq, Show, Generic, Hashable)

type TokenData  = HM.HashMap Token Integer
initialTData :: TokenData
initialTData = HM.fromList [(T1,0),(T2,0),(T3,0)]

type ZertzField = HM.HashMap Coord (Maybe Token)
data Player = P1 | P2
  deriving (Eq, Show, Generic, Hashable)  

data GamePhase = PlaceToken
               | RemoveField
               | ChooseJumpToken
               | CanJumpAgain
               | Jump
               | EndTurn
               | EndOfTheGame
               | Stop

  deriving (Eq, Show)

data TurnType = PlaceAndRemove | JumpAndEat
  deriving (Eq,Show)

data Zertz = Zertz {
  _field :: ZertzField,

  _playersTokens     :: HM.HashMap Player TokenData,
  _freeTokens        :: TokenData,

  _gamePhase         :: GamePhase,

  _selected          :: Coord,
  _activeJump        :: Maybe Coord,
  _activePlayer      :: Player,

  _possibleSelection :: [Coord]

  } deriving (Show)


$(makeLenses ''Zertz)


-- setting

t1 :: Integer
t1 = 5

t2 :: Integer
t2 = 7

t3 :: Integer
t3 = 9

fieldRad :: Integer
fieldRad = 3

startingTokenData :: TokenData
startingTokenData = HM.fromList [ (T1, 5), (T2, 7), (T3, 9) ]

emptyTokenData :: TokenData
emptyTokenData    = HM.fromList [ (T1, 0), (T2, 0), (T3, 0) ]

initialField :: ZertzField
initialField = HM.fromList [((x,y), Nothing) | x <- [-fieldRad .. fieldRad], y <- [-fieldRad ..fieldRad], x*y >= 0 || abs x + abs y <= fieldRad]

initialZertz :: Zertz
initialZertz = Zertz {
    _field = initialField,

    _playersTokens = HM.fromList $ ($ emptyTokenData) <$> [(P1,), (P2,)],
    _freeTokens = startingTokenData,

    _gamePhase = PlaceToken,

    _selected = (0,0),
    _activeJump = Nothing,
    _activePlayer = P1,

    _possibleSelection = []
  }



isWinner :: TokenData -> Bool
isWinner tk = or[any (> 0) res, all (>= 3) res']
  where
    f = (tk HM.!)
    g n = n `div` 2 + 1
    tlist = [T1, T2, T3]
    res = zipWith (-) (f <$> tlist) (g <$> [t1, t2, t3])
    res' = f <$> tlist

