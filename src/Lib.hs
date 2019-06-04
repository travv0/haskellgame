{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lib where

import           Apecs
import           Apecs.Gloss
import           Control.Monad
import           Data.Semigroup                 ( Semigroup )
import           Linear
import           System.Exit
import           System.Random
import qualified Data.Set                      as Set
import qualified Data.Vector                   as V

data Direction = L | R deriving (Eq, Show)

newtype Position = Position (V2 Float) deriving Show
instance Component Position where type Storage Position = Map Position

newtype Velocity = Velocity (V2 Float) deriving Show
instance Component Velocity where type Storage Velocity = Map Velocity

newtype Gravity = Gravity Float deriving Show
instance Component Gravity where type Storage Gravity = Map Gravity

data Enemy = Enemy deriving Show
instance Component Enemy where type Storage Enemy = Map Enemy

data Bullet = Bullet deriving Show
instance Component Bullet where type Storage Bullet = Map Bullet

data EnemyBullet = EnemyBullet Picture Float deriving Show
instance Component EnemyBullet where type Storage EnemyBullet = Map EnemyBullet

data Platform = Platform deriving Show
instance Component Platform where type Storage Platform = Map Platform

newtype Particle = Particle Float deriving Show
instance Component Particle where type Storage Particle = Map Particle

data Player = Player deriving Show
instance Component Player where type Storage Player = Unique Player

newtype Jumping = Jumping Float deriving Show
instance Component Jumping where type Storage Jumping = Map Jumping

data Hitbox = Hitbox (V2 Float) (V2 Float) deriving Show
instance Component Hitbox where type Storage Hitbox = Map Hitbox

data IsShooting = IsShooting Float Direction deriving Show
instance Component IsShooting where type Storage IsShooting = Map IsShooting

data CanJump = CanJump deriving Show
instance Component CanJump where type Storage CanJump = Map CanJump

data BulletPatternInterval = RandomInterval Float | FixedInterval Float Float deriving Show

data BulletPatternType = HomingPattern | RandomPattern (Float, Float) (Float, Float) (Float, Float) deriving Show

data BulletPattern = BulletPattern
  { bulletPatternInterval :: BulletPatternInterval
  , bulletPatternBulletPicture :: Picture
  , bulletPatternType :: BulletPatternType
  , bulletPatternBulletsPerStep :: Int
  , bulletPatternShootTime :: Float
  , bulletPatternStepTime :: Float
  } deriving Show

data ShootsPatterns = ShootsPatterns (V.Vector BulletPattern) deriving Show
instance Component ShootsPatterns where type Storage ShootsPatterns = Map ShootsPatterns

newtype Keys = Keys (Set.Set Key) deriving Show
instance Semigroup Keys where Keys s1 <> Keys s2 = Keys (s1 <> s2)
instance Monoid Keys where mempty = Keys Set.empty
instance Component Keys where type Storage Keys = Global Keys

newtype Score = Score Int deriving (Show, Num)
instance Semigroup Score where (<>) = (+)
instance Monoid Score where mempty = 0
instance Component Score where type Storage Score = Global Score

newtype Time = Time Float deriving (Show, Num)
instance Semigroup Time where (<>) = (+)
instance Monoid Time where mempty = 0
instance Component Time where type Storage Time = Global Time

makeWorld "World"
  [ ''Position
  , ''Velocity
  , ''Player
  , ''Enemy
  , ''Bullet
  , ''Score
  , ''Time
  , ''Particle
  , ''Camera
  , ''IsShooting
  , ''Gravity
  , ''Keys
  , ''Jumping
  , ''CanJump
  , ''Platform
  , ''Hitbox
  , ''ShootsPatterns
  , ''EnemyBullet
  ]

type System' a = System World a
type Kinetic = (Position, Velocity)

playerSpeed, playerBulletCooldown, jumpVelocity, playerJumpTime, gravity, bulletSpeed, enemySpeed, xmin, xmax, ymin, ymax, cooldownAdjust
  :: Float
playerSpeed = 170
playerBulletCooldown = 2
playerJumpTime = 30
jumpVelocity = 200
gravity = 750
bulletSpeed = 600
enemySpeed = 80
xmin = -230
xmax = 230
ymin = -170
ymax = 170
cooldownAdjust = 100

hitBonus, missPenalty :: Int
hitBonus = 100
missPenalty = 40

playerPos, scorePos :: V2 Float
playerPos = V2 0 (-120)
scorePos = V2 xmin (-170)

initialize :: System' ()
initialize = do
  _playerEty <- newEntity
    ( Player
    , Position playerPos
    , Velocity 0
    , Gravity gravity
    , Hitbox (V2 10 20) (V2 0 (-10))
    )
  _platformEty <- newEntity
    (Platform, Position $ playerPos + V2 5 (-25), Hitbox (V2 400 2) 0)
  return ()

stepPosition :: Float -> System' ()
stepPosition dT = cmap $ \(Position p, Velocity v) -> Position (p + dT *^ v)

stepVelocity :: Float -> System' ()
stepVelocity dT =
  cmap $ \(Velocity (V2 x y), Gravity g) -> Velocity (V2 x (y - dT * g))

clampPlayer :: System' ()
clampPlayer = cmap $ \(Player, Position (V2 x y)) ->
  Position (V2 (min xmax . max xmin $ x) (min ymax . max ymin $ y))

stepBulletPatterns :: Float -> System' ()
stepBulletPatterns dT = cmapM $ \(ShootsPatterns patterns, Position pos) ->
  fmap ShootsPatterns
    <$> V.forM patterns
    $   \ptn@BulletPattern {..} -> case bulletPatternInterval of
          RandomInterval rate              -> undefined
          FixedInterval currTime shootTime -> if currTime >= shootTime
            then do
              shootPattern (Position pos) ptn
              return ptn
            else return ptn
              { bulletPatternInterval = FixedInterval (currTime + 1 * dT) currTime
              }

shootPattern :: Position -> BulletPattern -> System' ()
shootPattern pos BulletPattern {..} = case bulletPatternType of
  RandomPattern dt dvx dvy -> replicateM_ bulletPatternBulletsPerStep $ do
    vx <- liftIO $ randomRIO dvx
    vy <- liftIO $ randomRIO dvy
    t  <- liftIO $ randomRIO dt
    newEntity
      (EnemyBullet bulletPatternBulletPicture t, pos, Velocity (V2 vx vy))
  HomingPattern -> undefined

playerJump :: Float -> System' ()
playerJump dT = cmap $ \(Player, Jumping jumpTime, Velocity (V2 x _)) ->
  if jumpTime > 0
    then Right
      (Velocity (V2 x jumpVelocity), Jumping (jumpTime - cooldownAdjust * dT))
    else Left $ Not @Jumping

playerShoot :: Float -> System' ()
playerShoot dT =
  cmapM_
    $ \(Player, pos, Velocity (V2 x y), IsShooting cooldown dir) ->
        if cooldown <= 0
          then do
            let dirMod = if dir == L then negate else id
            _ <- newEntity
              (Bullet, pos, Velocity (V2 (dirMod bulletSpeed + x) 0))
            spawnParticles 7
                           pos
                           (dirMod (10 + x * dT), dirMod (100 + x * dT))
                           (-80 + y * dT        , 80 + y * dT)
            cmap $ \Player -> IsShooting playerBulletCooldown dir
          else cmap $ \Player -> IsShooting (cooldown - cooldownAdjust * dT) dir

incrTime :: Float -> System' ()
incrTime dT = modify global $ \(Time t) -> Time (t + dT)

clearEnemys :: System' ()
clearEnemys = cmap $ \ent@(Enemy, Position (V2 x _), Velocity _) ->
  if x < xmin || x > xmax then Nothing else Just ent

stepParticles :: Float -> System' ()
stepParticles dT = cmap $ \(Particle t) ->
  if t < 0 then Right $ Not @(Particle, Kinetic) else Left $ Particle (t - dT)

stepBullets :: System' ()
stepBullets = cmap $ \(Bullet, Position (V2 _ py), Score s) -> if py > 170
  then Right (Not @(Bullet, Kinetic), Score (s - missPenalty))
  else Left ()

handleCollisions :: Float -> System' ()
handleCollisions dT = do
  cmapM_ $ \(Enemy, Position posT, etyT) ->
    cmapM_ $ \(Bullet, Position posB, etyB) ->
      when (norm (posT - posB) < 10) $ do
        destroy etyT (Proxy @(Enemy, Kinetic))
        destroy etyB (Proxy @(Bullet, Kinetic))
        spawnParticles 15 (Position posB) (-200, 200) (-200, 200)
        modify global $ \(Score x) -> Score (x + hitBonus)

  cmapM_
    $ \(Player, Position (V2 xP yP), Hitbox (V2 widthP heightP) (V2 osxP osyP), Velocity (V2 velxP velyP), etyP) ->
        cmapM_
          $ \(Platform, Position (V2 xF yF), Hitbox (V2 widthF heightF) (V2 osxF osyF)) ->
              let playerBottom  = yP - heightP / 2 + osyP
                  playerRight   = xP + widthP / 2 + osxP
                  playerLeft    = xP - widthP / 2 + osxP
                  platformLeft  = xF - widthF / 2 + osxF
                  platformRight = xF + widthF / 2 + osxF
                  platformTop   = yF + heightF / 2 + osyF
              in  when
                    (  (playerRight >= platformLeft)
                    && (playerLeft <= platformRight)
                    && (playerBottom >= platformTop)
                    && (playerBottom + velyP * dT <= platformTop)
                    )
                  $  etyP
                  $= ( Position
                       (V2 xP (yF + heightF / 2 + osyF + heightP + 1))
                     , Velocity (V2 velxP 0)
                     , CanJump
                     )

triggerEvery :: Float -> Float -> Float -> System' a -> System' ()
triggerEvery dT period phase sys = do
  Time t <- get global
  let t'      = t + phase
      trigger = floor (t' / period) /= (floor ((t' + dT) / period) :: Integer)
  when trigger $ void sys

spawnParticles
  :: Int -> Position -> (Float, Float) -> (Float, Float) -> System' ()
spawnParticles n pos dvx dvy = replicateM_ n $ do
  vx <- liftIO $ randomRIO dvx
  vy <- liftIO $ randomRIO dvy
  t  <- liftIO $ randomRIO (0.02, 0.3)
  newEntity (Particle t, pos, Velocity (V2 vx vy))

step :: Float -> System' ()
step dT = do
  incrTime dT
  playerJump dT
  playerShoot dT
  stepBulletPatterns dT
  handleCollisions dT
  stepVelocity dT
  stepPosition dT
  clampPlayer
  clearEnemys
  stepBullets
  stepParticles dT
  triggerEvery dT 0.6 0 $ newEntity
    ( Enemy
    , Position (V2 xmin 80)
    , Velocity (V2 enemySpeed 0)
    , ShootsPatterns $ V.fromList
      [ BulletPattern
          { bulletPatternInterval       = FixedInterval 5 5
          , bulletPatternBulletPicture  = diamond
          , bulletPatternType           = RandomPattern (3, 5) (-5, 5) (-5, 5)
          , bulletPatternBulletsPerStep = 3
          , bulletPatternShootTime      = 10
          , bulletPatternStepTime       = 10
          }
      ]
    )
  triggerEvery dT 0.6 0.3 $ newEntity
    (Enemy, Position (V2 xmax 120), Velocity (V2 (negate enemySpeed) 0))

handleInput :: Event -> System' Event
handleInput event@(EventKey k Down _ _) = do
  modify global $ \(Keys keys) -> Keys $ Set.insert k keys
  return event
handleInput event@(EventKey k Up _ _) = do
  modify global $ \(Keys keys) -> Keys $ Set.delete k keys
  return event
handleInput event = return event

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
  cmap $ \(Player, CanJump) -> (Jumping playerJumpTime, Not @CanJump)

handleEvent (EventKey (SpecialKey KeyUp) Up _ _) =
  cmap $ \(Player, Jumping _) -> Not @Jumping

handleEvent (EventKey (Char 'x') Down _ _) = do
  cmap $ \Player -> IsShooting 0 R
  cmap $ \(Player, IsShooting cooldown _) -> IsShooting cooldown R

handleEvent (EventKey (Char 'x') Up _ _) =
  cmapM_
    $ \(Player, IsShooting timeout _, Keys keys, e) ->
        if Set.member (Char 'z') keys
          then set e (IsShooting timeout L)
          else destroy e (Proxy @IsShooting)

handleEvent (EventKey (Char 'z') Down _ _) = do
  cmap $ \Player -> IsShooting 0 L
  cmap $ \(Player, IsShooting cooldown _) -> IsShooting cooldown L

handleEvent (EventKey (Char 'z') Up _ _) =
  cmapM_
    $ \(Player, IsShooting timeout _, Keys keys, e) ->
        if Set.member (Char 'x') keys
          then set e (IsShooting timeout R)
          else destroy e (Proxy @IsShooting)

handleEvent (EventKey (SpecialKey KeyEsc) Down _ _) = liftIO exitSuccess

handleEvent _ = return ()

translate' :: Position -> Picture -> Picture
translate' (Position (V2 x y)) = translate x y

playerPic, diamond, box :: Picture
playerPic = Line [(0, 0), (-0.5, -1), (0.5, -1), (0, 0)]
diamond = Line [(-1, 0), (0, -1), (1, 0), (0, 1), (-1, 0)]
box = Line [(0.5, 0.5), (-0.5, 0.5), (-0.5, -0.5), (0.5, -0.5), (0.5, 0.5)]

draw :: System' Picture
draw = do
  player <- foldDraw
    $ \(Player, pos) -> translate' pos . color white . scale 10 20 $ playerPic
  enemys <- foldDraw
    $ \(Enemy, pos) -> translate' pos . color white . scale 10 10 $ diamond
  bullets <- foldDraw
    $ \(Bullet, pos) -> translate' pos . color white . scale 4 4 $ diamond
  enemyBullets <- foldDraw $ \(EnemyBullet pic _, pos) ->
    translate' pos . color white . scale 4 4 $ pic
  platforms <- foldDraw
    $ \(Platform, pos) -> translate' pos . color white . scale 400 2 $ box
  hitboxes <- foldDraw $ \(Hitbox (V2 w h) offset, Position pos) ->
    translate' (Position $ pos + offset) . color yellow . scale w h $ box

  particles <- foldDraw $ \(Particle _, Velocity (V2 vx vy), pos) ->
    translate' pos . color white $ Line [(0, 0), (vx / 10, vy / 10)]

  Score s <- get global
  let score =
        color white
          .  translate' (Position scorePos)
          .  scale 0.1 0.1
          .  Text
          $  "Score: "
          ++ show s

  return
    $  player
    <> enemys
    <> bullets
    <> score
    <> particles
    <> platforms
    <> enemyBullets
    -- <> hitboxes
