{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lib where

import           Apecs
import           Apecs.Physics.Gloss
import           Apecs.Physics
import           Control.Monad
import           Data.Semigroup                 ( Semigroup )
import           GHC.Float
import           Linear
import           System.Exit
import           System.Random

data Target = Target deriving Show
instance Component Target where type Storage Target = Map Target

data Bullet = Bullet deriving Show
instance Component Bullet where type Storage Bullet = Map Bullet

newtype Particle = Particle Double deriving Show
instance Component Particle where type Storage Particle = Map Particle

data Player = Player deriving Show
instance Component Player where type Storage Player = Unique Player

newtype Score = Score Int deriving (Show, Num)
instance Semigroup Score where (<>) = (+)
instance Monoid Score where mempty = 0
instance Component Score where type Storage Score = Global Score

newtype Time = Time Double deriving (Show, Num)
instance Semigroup Time where (<>) = (+)
instance Monoid Time where mempty = 0
instance Component Time where type Storage Time = Global Time

makeWorld "World"
  [ ''Player
  , ''Target
  , ''Bullet
  , ''Score
  , ''Time
  , ''Particle
  , ''Camera
  , ''Physics
  ]

type System' a = System World a

playerSpeed, bulletSpeed, enemySpeed, xmin, xmax :: Double
playerSpeed = 170
bulletSpeed = 400
enemySpeed = 80
xmin = -230
xmax = 230

hitBonus, missPenalty :: Int
hitBonus = 100
missPenalty = 40

playerPos, scorePos :: V2 Double
playerPos = V2 0 120
scorePos = V2 xmin (-170)

initialize :: System' ()
initialize = do
  set global earthGravity
  _playerEty <- newEntity
    (Player, DynamicBody, BodyMass 50, Moment 1, Position playerPos)
  return ()

clampPlayer :: System' ()
clampPlayer = cmap
  $ \(Player, Position (V2 x y)) -> Position (V2 (min xmax . max xmin $ x) y)

incrTime :: Float -> System' ()
incrTime dT = modify global $ \(Time t) -> Time (t + float2Double dT)

clearTargets :: System' ()
clearTargets =
  cmap
    $ \ent@(Target, Position (V2 x _)) -> if x < (xmin - 20) || x > (xmax + 20)
        then Right $ Not @(Target, Body)
        else Left ent

stepParticles :: Float -> System' ()
stepParticles dT = cmap $ \(Particle t) -> if t < 0
  then Right $ Not @(Particle, Body)
  else Left $ Particle (t - float2Double dT)

stepBullets :: System' ()
stepBullets = cmap $ \(Bullet, Position (V2 _ py), Score s) -> if py > 170
  then Right (Not @(Bullet, Body), Score (s - missPenalty))
  else Left ()

handleCollisions :: System' ()
handleCollisions = cmapM_ $ \(Target, Position posT, etyT) ->
  cmapM_ $ \(Bullet, Position posB, etyB) -> when (norm (posT - posB) < 10) $ do
    destroy etyT (Proxy @(Target, Body))
    destroy etyB (Proxy @(Bullet, Body))
    spawnParticles 15 (Position posB) (-500, 500) (200, -50)
    modify global $ \(Score x) -> Score (x + hitBonus)

triggerEvery :: Float -> Double -> Double -> System' a -> System' ()
triggerEvery dT period phase sys = do
  Time t <- get global
  let t' = t + phase
      trigger =
        floor (t' / period)
          /= (floor ((t' + float2Double dT) / period) :: Integer)
  when trigger $ void sys

spawnParticles
  :: Int -> Position -> (Double, Double) -> (Double, Double) -> System' ()
spawnParticles n pos dvx dvy = replicateM_ n $ do
  vx <- liftIO $ randomRIO dvx
  vy <- liftIO $ randomRIO dvy
  t  <- liftIO $ randomRIO (0.02, 0.3)
  newEntity (Particle t, KinematicBody, pos, Velocity (V2 vx vy))

step :: Float -> System' ()
step dT = do
  incrTime dT
  stepPhysics (float2Double dT)
  clampPlayer
  clearTargets
  stepBullets
  stepParticles dT
  handleCollisions
  triggerEvery dT 0.6 0 $ newEntity
    ( Target
    , KinematicBody
    , Position (V2 (xmin - 20) 80)
    , Velocity (V2 enemySpeed 0)
    )
  triggerEvery dT 0.6 0.3 $ newEntity
    ( Target
    , KinematicBody
    , Position (V2 (xmax + 20) 120)
    , Velocity (V2 (negate enemySpeed) 0)
    )

handleEvent :: Event -> System' ()
handleEvent (EventKey (SpecialKey KeyLeft) Down _ _) =
  cmap $ \(Player, Velocity (V2 x y)) -> Velocity (V2 (x - playerSpeed) y)

handleEvent (EventKey (SpecialKey KeyLeft) Up _ _) =
  cmap $ \(Player, Velocity (V2 x y)) -> Velocity (V2 (x + playerSpeed) y)

handleEvent (EventKey (SpecialKey KeyRight) Down _ _) =
  cmap $ \(Player, Velocity (V2 x y)) -> Velocity (V2 (x + playerSpeed) y)

handleEvent (EventKey (SpecialKey KeyRight) Up _ _) =
  cmap $ \(Player, Velocity (V2 x y)) -> Velocity (V2 (x - playerSpeed) y)

handleEvent (EventKey (SpecialKey KeyUp) Down _ _) =
  cmap $ \(Player, Velocity (V2 x _)) -> Velocity (V2 x 10)

handleEvent (EventKey (SpecialKey KeySpace) Down _ _) =
  cmapM_ $ \(Player, pos, Velocity (V2 x _)) -> do
    _ <- newEntity (Bullet, KinematicBody, pos, Velocity (V2 x bulletSpeed))
    spawnParticles 7 pos (-80, 80) (10, 100)

handleEvent (EventKey (SpecialKey KeyEsc) Down _ _) = liftIO exitSuccess

handleEvent _ = return ()

translate' :: Position -> Picture -> Picture
translate' (Position (V2 x y)) = translate (double2Float x) (double2Float y)

triangle, diamond :: Picture
triangle = Line [(0, 0), (-0.5, -1), (0.5, -1), (0, 0)]
diamond = Line [(-1, 0), (0, -1), (1, 0), (0, 1), (-1, 0)]

draw :: System' Picture
draw = do
  player <- foldDraw
    $ \(Player, pos) -> translate' pos . color white . scale 10 20 $ triangle
  targets <- foldDraw
    $ \(Target, pos) -> translate' pos . color white . scale 10 10 $ diamond
  bullets <- foldDraw
    $ \(Bullet, pos) -> translate' pos . color white . scale 4 4 $ diamond

  particles <- foldDraw $ \(Particle _, Velocity (V2 vx vy), pos) ->
    translate' pos . color white $ Line
      [(0, 0), (double2Float vx / 10, double2Float vy / 10)]

  Score s <- get global
  let score =
        color white
          .  translate' (Position scorePos)
          .  scale 0.1 0.1
          .  Text
          $  "Score: "
          ++ show s

  return $ player <> targets <> bullets <> score <> particles
