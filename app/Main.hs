{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE MultiWayIf #-}

module Main where

import Brick
import Brick.Widgets.Center (center)
import Brick.Widgets.Table
import Control.Monad
import Control.Monad.State
import Data.Bifunctor
import Data.Foldable
import Data.Generics.Labels
import Data.List
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Vector as V
import Data.Vector ((!), Vector)
import Debug.Trace
import GHC.Generics
import qualified Graphics.Vty as Vty
import Graphics.Vty
  ( black,
    blue,
    cyan,
    green,
    red,
    white,
    yellow,
  )
import Immutable.Shuffle
import Lens.Micro
import System.Random
import Util (chunksOf, singleton)

data Position = Mine | Clear

type Loc = (Int, Int)

data PosDisplay = Covered | Exposed Int | Flagged | Explosion

data Cursor = Cursor {row :: Int, col :: Int}
  deriving (Generic)

data AppState
  = AppState
      { internal :: Vector (Vector Position),
        display :: Vector (Vector PosDisplay),
        cursor :: Cursor
      }
  deriving (Generic)

adjacentTransformations =
  [ ((+ 1), id),
    ((+ 1), (+ 1)),
    ((+ 1), subtract 1),
    (id, (+ 1)),
    (id, subtract 1),
    (subtract 1, id),
    (subtract 1, subtract 1),
    (subtract 1, (+ 1))
  ]

numWidget :: Int -> Widget ()
numWidget = str . show

displayPos :: PosDisplay -> Widget ()
displayPos (Exposed x)
  | x == 0 = txt " "
  | otherwise = withAttr "num" $ numWidget x
displayPos Flagged = txt "!"
displayPos Explosion = txt "*"
displayPos Covered = withAttr "covered" $ txt "?"

genBoard :: Int -> Int -> IO (Vector (Vector Position))
genBoard size mines = chunksOf size <$> shuffleM b
  where
    b = V.replicate mines Mine <> V.replicate (size * size - mines) Clear

applyTransform :: Loc -> (Int -> Int, Int -> Int) -> Loc
applyTransform = flip (uncurry bimap)

search :: (Loc -> Bool) -> (Loc -> Bool) -> Loc -> Set Loc -> Set Loc
search noVisit noRecurse node visited
  | noVisit node = visited
  | node `S.member` visited = visited
  | noRecurse node = node `S.insert` visited
  | otherwise =
    let comp = compose $ search noVisit noRecurse <$> neighbors node
     in comp (node `S.insert` visited)
  where
    neighbors o = applyTransform o <$> adjacentTransformations
    compose = flip (foldl' (flip id))

numSurrounding :: Loc -> Vector (Vector Position) -> Int
numSurrounding l g = length $ filter isMine $ filter isValid $ nub $ applyTransform l <$> adjacentTransformations
  where
    isValid (r, c) =
      let r' = clamp 0 (length g - 1) r
          c' = clamp 0 (length (g ! 0) - 1) c
       in (r', c') == (r, c)
    isMine (r, c) = case g ! r ! c of
      Mine -> True
      _ -> False

imap2d :: (Int -> Int -> a -> a) -> Vector (Vector a) -> Vector (Vector a)
imap2d f = V.imap (V.imap . f)
-- Explicit: imap2d f = V.imap (\i v -> V.imap (\j e -> f i j e) v)

vModifyValue2d :: Int -> Int -> (a -> a) -> Vector (Vector a) -> Vector (Vector a)
vModifyValue2d i j f = imap2d (\a b -> if | (a, b) == (i, j) -> f
                                          | otherwise        -> id)

explore :: AppState -> Vector (Vector PosDisplay)
explore (AppState int d (Cursor r c)) =
  let affectedPositions = search noVisit noRecurse (r, c) S.empty
   in imap2d
        ( \i j ->
            if | not ((i, j) `S.member` affectedPositions) -> id
               | otherwise -> const (Exposed (numSurrounding (i, j) int))
        )
        d
  where
    noVisit (a, b) =
      let a' = clamp 0 (length int - 1) a
          b' = clamp 0 (length (int ! 0) - 1) b
       in (a', b') /= (a, b) || case int ! a' ! b' of
            Mine -> True
            _ -> case d ! a' ! b' of
              Exposed _ -> True
              _ -> False
    noRecurse pos
      | numSurrounding pos int == 0 = False
      | otherwise = True
    addIndices = fmap (\(a, b) -> (a, (a, b)))
    isMine (r, c) = case int ! r ! c of
      Mine -> True
      _ -> False

appEvent :: AppState -> BrickEvent n () -> EventM n (Next AppState)
appEvent s (VtyEvent (Vty.EvKey Vty.KEsc [])) = halt s
appEvent s (VtyEvent (Vty.EvKey (Vty.KChar ' ') [])) = case state of
  Exposed _ -> continue s
  Covered | not isMine -> continue $ s {display = explore s}
  Covered | isMine -> continue $ s { display = vModifyValue2d curRow curCol (const Explosion) (display s) }
  where
    cs = s ^. #cursor
    curRow = row cs
    curCol = col cs
    loc = internal s ! curRow ! curCol
    isMine = case loc of
      Mine -> True
      _    -> False
    state = display s ! curRow ! curCol
appEvent s@(AppState i d (Cursor r c)) (VtyEvent (Vty.EvKey k []))
  | k == Vty.KChar 'k' = continue $ updateCursor (subtract 1) id s
  | k == Vty.KChar 'j' = continue $ updateCursor (+ 1) id s
  | k == Vty.KChar 'l' = continue $ updateCursor id (+ 1) s
  | k == Vty.KChar 'h' = continue $ updateCursor id (subtract 1) s
  where
    clampCoord =
      ( clamp 0 (length i - 1),
        clamp 0 (length (i ! 0) - 1)
      )
    updateCursor rf cf state =
      state & #cursor
        %~ ( (#row %~ (fst clampCoord . rf))
               . (#col %~ (snd clampCoord . cf))
           )
appEvent s _ =
  continue s

drawUI :: AppState -> [Widget ()]
drawUI (AppState int disp (Cursor r c)) =
  singleton . center . renderTable . table . (ix r . ix c %~ withAttr "current") . V.toList $
    V.toList
      <$> fmap (fmap (padLeftRight 1 . displayPos)) disp

attrs =
  [ ("current", bg blue)
  , ("covered", fg green)
  , ("num",     fg yellow)
  ]

app =
  App
    { appDraw = drawUI,
      appHandleEvent = appEvent,
      appChooseCursor = neverShowCursor,
      appStartEvent = return,
      appAttrMap = const $ attrMap Vty.defAttr attrs
    }

main :: IO ()
main = do
  let sz = 15
      mines = 20
  board <- genBoard sz mines
  let disp = V.replicate sz (V.replicate sz Covered)
  defaultMain app (AppState board disp (Cursor 0 0)) >> pure ()
