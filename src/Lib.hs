{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}

module Lib where

import           Apecs
import           Apecs.Gloss
import           Control.Monad
import           Data.Char
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

data DangerZone = DangerZone deriving Show
instance Component DangerZone where type Storage DangerZone = Unique DangerZone

data Focus = Focus deriving Show
instance Component Focus where type Storage Focus = Unique Focus

data EnemyBullet = EnemyBullet Picture Float Float deriving Show
instance Component EnemyBullet where type Storage EnemyBullet = Map EnemyBullet

data Platform = Platform deriving Show
instance Component Platform where type Storage Platform = Map Platform

newtype Particle = Particle Float deriving Show
instance Component Particle where type Storage Particle = Map Particle

data Player = Player deriving Show
instance Component Player where type Storage Player = Unique Player

data MovingUp = MovingUp deriving Show
instance Component MovingUp where type Storage MovingUp = Unique MovingUp
data MovingRight = MovingRight deriving Show
instance Component MovingRight where type Storage MovingRight = Unique MovingRight
data MovingDown = MovingDown deriving Show
instance Component MovingDown where type Storage MovingDown = Unique MovingDown
data MovingLeft = MovingLeft deriving Show
instance Component MovingLeft where type Storage MovingLeft = Unique MovingLeft

newtype Jumping = Jumping Float deriving Show
instance Component Jumping where type Storage Jumping = Unique Jumping

data Hitbox = Hitbox (V2 Float) (V2 Float) deriving Show
instance Component Hitbox where type Storage Hitbox = Map Hitbox

data Hitpoint = Hitpoint (V2 Float) (V2 Float) deriving Show
instance Component Hitpoint where type Storage Hitpoint = Unique Hitpoint

data IsShooting = IsShooting Float Direction deriving Show
instance Component IsShooting where type Storage IsShooting = Unique IsShooting

data CanJump = CanJump deriving Show
instance Component CanJump where type Storage CanJump = Unique CanJump

data BulletPatternInterval = FixedInterval Float Float deriving Show

data BulletPatternType = RandomPattern (Float, Float) (Float, Float) (Float, Float) deriving Show

data BulletPatternShootTimes = ShootTimes Float Float deriving Show

data BulletPattern = BulletPattern
  { bulletPatternInterval :: BulletPatternInterval
  , bulletPatternBulletPicture :: Picture
  , bulletPatternType :: BulletPatternType
  , bulletPatternBulletsPerStep :: Int
  , bulletPatternShootTime :: Float
  , bulletPatternShootTimes :: BulletPatternShootTimes
  } deriving Show

newtype ShootsPatterns = ShootsPatterns (V.Vector BulletPattern) deriving Show
instance Component ShootsPatterns where type Storage ShootsPatterns = Map ShootsPatterns

newtype Keys = Keys (Set.Set Key) deriving Show
instance Semigroup Keys where Keys s1 <> Keys s2 = Keys (s1 <> s2)
instance Monoid Keys where mempty = Keys Set.empty
instance Component Keys where type Storage Keys = Global Keys

newtype Score = Score Float deriving (Show, Num)
instance Semigroup Score where (<>) = (+)
instance Monoid Score where mempty = 0
instance Component Score where type Storage Score = Global Score

newtype HighScore = HighScore Float deriving (Show, Num)
instance Semigroup HighScore where (<>) = (+)
instance Monoid HighScore where mempty = 0
instance Component HighScore where type Storage HighScore = Global HighScore

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
  , ''HighScore
  , ''Camera
  , ''Time
  , ''Particle
  , ''IsShooting
  , ''Gravity
  , ''Keys
  , ''Jumping
  , ''CanJump
  , ''Platform
  , ''Hitbox
  , ''Hitpoint
  , ''ShootsPatterns
  , ''EnemyBullet
  , ''DangerZone
  , ''Focus
  , ''MovingUp
  , ''MovingRight
  , ''MovingDown
  , ''MovingLeft
  ]

type System' a = System World a
type Kinetic = (Position, Velocity)

playerSpeed, playerBulletCooldown, jumpVelocity, playerJumpTime, gravity, bulletSpeed, xmin, xmax, ymin, ymax, cooldownAdjust, scrollSpeed, scoreTimeMod, dangerZoneRange
  :: Float
playerSpeed = 170
playerBulletCooldown = 2
playerJumpTime = 30
jumpVelocity = 200
gravity = 750
bulletSpeed = 600
xmin = -230
xmax = 230
ymin = -170
ymax = 170
cooldownAdjust = 100
scrollSpeed = 50
scoreTimeMod = 10
dangerZoneRange = 60

hitBonus :: Int
hitBonus = 100

playerPos, scorePos, highScorePos :: V2 Float
playerPos = V2 0 (-120)
scorePos = V2 xmin (-170)
highScorePos = V2 xmin 165

initialize :: System' ()
initialize = do
  _playerEty <- newEntity
    ( Player
    , Position playerPos
    , Velocity 0
    , Gravity gravity
    , Hitbox (V2 10 20) (V2 0 (-10))
    , Hitpoint (V2 10 20) (V2 0 (-12))
    )
  _platformEty <- newEntity
    (Platform, Position $ playerPos + V2 5 (-25), Hitbox (V2 400 2) 0)
  return ()

resetGame :: System' ()
resetGame = do
  global $~ \(Score _) -> Score 0
  cmapM_ $ \(Player, ety) ->
    destroy ety
      $ Proxy
        @( Player
        , Kinetic
        , IsShooting
        , Gravity
        , Jumping
        , CanJump
        , Hitbox
        , DangerZone
        )
  cmapM_
    $ \(Enemy, ety) -> destroy ety $ Proxy @(Enemy, Kinetic, ShootsPatterns)
  cmapM_ $ \(Bullet, ety) -> destroy ety $ Proxy @(Bullet, Kinetic)
  cmapM_ $ \(EnemyBullet{}, ety) -> destroy ety $ Proxy @(EnemyBullet, Kinetic)
  cmapM_ $ \(Platform, ety) -> destroy ety $ Proxy @(Platform, Kinetic)
  initialize

stepScroll :: Float -> System' ()
stepScroll dT =
  cmap $ \(Position (V2 x y), Not :: Not Player, Not :: Not Particle) ->
    Position (V2 (x - scrollSpeed * dT) y)

stepPosition :: Float -> System' ()
stepPosition dT = cmap $ \(Position p, Velocity v) -> Position (p + dT *^ v)

stepHitpoint :: System' ()
stepHitpoint = cmap $ \(Player, Position (V2 x y), Hitpoint _ (V2 dx dy)) ->
  Hitpoint (V2 (x + dx) (y + dy)) (V2 dx dy)

stepVelocity :: Float -> System' ()
stepVelocity dT =
  cmap $ \(Velocity (V2 x y), Gravity g, Not :: Not DangerZone) ->
    Velocity (V2 x (y - dT * g))

clampPlayer :: System' ()
clampPlayer =
  cmapM $ \(Player, Position (V2 x y), ety) -> if x < xmin - 20 || y < ymin - 20
    then resetGame
    else ety $= Position (V2 (min xmax x) (min ymax y))

stepBulletPatterns :: Float -> System' ()
stepBulletPatterns dT = cmapM $ \(ShootsPatterns patterns, Position pos) ->
  fmap ShootsPatterns <$> V.forM patterns $ \ptn@BulletPattern {..} ->
    case bulletPatternInterval of
      FixedInterval currTime shootTime -> if currTime >= shootTime
        then
          let ShootTimes curr final = bulletPatternShootTimes
          in
            if curr < final
              then do
                shootPattern (Position pos) ptn
                return ptn
                  { bulletPatternInterval   =
                    FixedInterval (shootTime - bulletPatternShootTime) shootTime
                  , bulletPatternShootTimes = ShootTimes (curr + 1) final
                  }
              else return ptn
                { bulletPatternInterval   = FixedInterval 0 shootTime
                , bulletPatternShootTimes = ShootTimes 0 final
                }
        else return ptn
          { bulletPatternInterval = FixedInterval (currTime + 1 * dT) shootTime
          }

stepEnemyBullets :: Float -> System' ()
stepEnemyBullets dT = cmap $ \(EnemyBullet p s t, Position (V2 px py)) ->
  if (t <= 0)
       || (py > ymax + threshold || py < ymin - threshold)
       || (px > xmax + threshold || px < xmin - threshold)
    then Left $ Not @(EnemyBullet, Kinetic)
    else Right $ EnemyBullet p s (t - 1 * dT)
  where threshold = 20

shootPattern :: Position -> BulletPattern -> System' ()
shootPattern pos BulletPattern {..} = case bulletPatternType of
  RandomPattern dt dvx dvy -> replicateM_ bulletPatternBulletsPerStep $ do
    vx <- liftIO $ randomRIO dvx
    vy <- liftIO $ randomRIO dvy
    t  <- liftIO $ randomRIO dt
    newEntity
      (EnemyBullet bulletPatternBulletPicture 4 t, pos, Velocity (V2 vx vy))

playerJump :: Float -> System' ()
playerJump dT = cmap $ \(Player, Jumping jumpTime, Velocity (V2 x _)) ->
  if jumpTime > 0
    then Right
      (Velocity (V2 x jumpVelocity), Jumping (jumpTime - cooldownAdjust * dT))
    else Left $ Not @Jumping

playerShoot :: Float -> System' ()
playerShoot dT =
  cmapM_
    $ \(Player, Position pos, Velocity (V2 x _), IsShooting cooldown dir) ->
        if cooldown <= 0
          then do
            let dirMod = if dir == L then negate else id
            _ <- newEntity
              (Bullet, Position pos, Velocity (V2 (dirMod bulletSpeed + x) 0))
            cmap $ \Player -> IsShooting playerBulletCooldown dir
          else cmap $ \Player -> IsShooting (cooldown - cooldownAdjust * dT) dir

incrTime :: Float -> System' ()
incrTime dT = modify global $ \(Time t) -> Time (t + dT)

scoreStep :: Float -> System' ()
scoreStep dT = cmapM $ \(Player, Position posP) -> do
  scoreBonus <- cfold
    (\acc (EnemyBullet{}, Position posE) ->
      acc + if norm (posP - posE) < dangerZoneRange then 1 else 0
    )
    0
  modify global $ \(Score s, HighScore hs) ->
    ( Score (s + dT * scoreTimeMod * scoreBonus)
    , HighScore (if s > hs then s else hs)
    )

clearEnemys :: System' ()
clearEnemys = cmap $ \ent@(Enemy, Position (V2 x _), Velocity _) ->
  if x < xmin || x > xmax then Nothing else Just ent

stepParticles :: Float -> System' ()
stepParticles dT = cmap $ \(Particle t) ->
  if t < 0 then Right $ Not @(Particle, Kinetic) else Left $ Particle (t - dT)

stepBullets :: System' ()
stepBullets = cmap $ \(Bullet, Position (V2 px py)) ->
  if (py > ymax + threshold || py < ymin - threshold)
       || (px > xmax + threshold || px < xmin - threshold)
    then Right (Not @(Bullet, Kinetic))
    else Left ()
  where threshold = 20

canDangerZone :: System' Bool
canDangerZone = flip cfoldM False $ \acc (Player, Position posP) -> do
  inDZ <- cfold
    (\innerAcc (EnemyBullet{}, Position posE) ->
      innerAcc || (norm (posP - posE) < dangerZoneRange)
    )
    False
  return $ acc || inDZ

handleCollisions :: Float -> System' ()
handleCollisions dT = do
  cmapM_ $ \(Enemy, Position posT, etyT) ->
    cmapM_ $ \(Bullet, Position posB, etyB) ->
      when (norm (posT - posB) < 10) $ do
        destroy etyT (Proxy @(Enemy, Kinetic))
        destroy etyB (Proxy @(Bullet, Kinetic))
        spawnParticles 50 (Position posB) (-200, 200) (-200, 200)
        global $~ \(Score x) -> Score (x + fromIntegral hitBonus)

  cmapM_ $ \(Player, Hitpoint hpP _, etyP, Velocity velP) -> do
    canDZ        <- canDangerZone
    isDangerZone <- exists etyP $ Proxy @DangerZone
    let leaveDangerZone = when isDangerZone $ do
          etyP $= Not @DangerZone
          etyP $= Velocity (velP * 2)
    if canDZ || (canDZ && not isDangerZone)
      then do
        spawnParticles 5 (Position hpP) (-20, 20) (-20, 20)
        etyP $= DangerZone
      else leaveDangerZone

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
              in  do
                    movingDown <- exists etyP $ Proxy @MovingDown
                    when
                        (  (playerRight >= platformLeft)
                        && (playerLeft <= platformRight)
                        && (playerBottom > platformTop)
                        && (playerBottom + velyP * dT <= platformTop)
                        && not movingDown
                        )
                      $  etyP
                      $= ( Position
                           (V2 xP (yF + heightF / 2 + osyF + heightP + 1))
                         , Velocity (V2 velxP 0)
                         , CanJump
                         )

  reset <- cfoldM
    (\acc (Player, Hitpoint posP _) -> do
      r <- cfold
        (\innerAcc (EnemyBullet _ size t, Position posB) ->
          innerAcc || t > dT * 2 && norm (posP - posB) < size
        )
        False
      return $ acc || r
    )
    False

  when reset resetGame

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

spawnEnemies :: Float -> System' ()
spawnEnemies dT = do
  enemyy               <- liftIO $ randomRIO (ymin, ymax)
  enemyRate            <- liftIO $ randomRIO (1 :: Float, 5000)
  enemyVel             <- liftIO $ randomRIO (-10 :: Float, -100)
  enemyInterval        <- liftIO $ randomRIO (1 :: Float, 5)
  enemyInitialInterval <- liftIO $ randomRIO (0, enemyInterval)
  let
    newEnemy = newEntity
      ( Enemy
      , Position (V2 xmax enemyy)
      , Velocity (V2 enemyVel 0)
      , ShootsPatterns $ V.fromList
        [ BulletPattern
            { bulletPatternInterval       = FixedInterval enemyInitialInterval
                                                          enemyInterval
            , bulletPatternBulletPicture  = diamond
            , bulletPatternType = RandomPattern (5, 10) (-100, 100) (-100, 100)
            , bulletPatternBulletsPerStep = 3
            , bulletPatternShootTime      = 0.2
            , bulletPatternShootTimes     = ShootTimes 0 10
            }
        ]
      )
  anyEnemies <- cfold (\acc Enemy -> acc || True) False
  when (not anyEnemies || enemyRate * dT < 1) $ void newEnemy

spawnPlatforms :: Float -> System' ()
spawnPlatforms dT = do
  platformy           <- liftIO $ randomRIO (ymin, 10)
  platformLength      <- liftIO $ randomRIO (10, xmax)
  platformSpawnChance <- liftIO
    $ randomRIO (platformLength, platformLength + xmax)
  when (platformSpawnChance * dT < 1) $ void $ newEntity
    (Platform, Position (V2 xmax platformy), Hitbox (V2 platformLength 2) 0)

step :: Float -> System' ()
step dT = do
  incrTime dT
  scoreStep dT
  stepPlayerMovement
  playerJump dT
  playerShoot dT
  stepBulletPatterns dT
  stepVelocity dT
  stepPosition dT
  stepHitpoint
  handleCollisions dT
  stepScroll dT
  clampPlayer
  clearEnemys
  stepBullets
  stepEnemyBullets dT
  stepParticles dT
  spawnEnemies dT
  -- spawnPlatforms dT

stepPlayerMovement :: System' ()
stepPlayerMovement = do
  cmap
    $ \(Player, Velocity (V2 _ y), Not :: Not DangerZone) -> Velocity (V2 0 y)
  cmap $ \(Player, DangerZone) -> Velocity (V2 0 0)

  cmapM $ \(Player, MovingLeft, Velocity (V2 x y), etyP) -> do
    isFocussed <- exists etyP $ Proxy @Focus
    return $ Velocity (V2 (x - playerSpeed * if isFocussed then 0.5 else 1) y)

  cmapM $ \(Player, MovingRight, Velocity (V2 x y), etyP) -> do
    isFocussed <- exists etyP $ Proxy @Focus
    return $ Velocity (V2 (x + playerSpeed * if isFocussed then 0.5 else 1) y)

  cmapM_ $ \(Player, MovingUp, Velocity (V2 x y), etyP) -> do
    isFocussed   <- exists etyP $ Proxy @Focus
    isDangerZone <- exists etyP $ Proxy @DangerZone
    if isDangerZone
      then etyP
        $= Velocity (V2 x (y + playerSpeed * if isFocussed then 0.5 else 1))
      else do
        canJump <- exists etyP $ Proxy @CanJump
        when canJump $ etyP $= (Jumping playerJumpTime, Not @CanJump)

  cmapM $ \(Player, MovingDown, DangerZone, Velocity (V2 x y), etyP) -> do
    isFocussed <- exists etyP $ Proxy @Focus
    return $ Velocity (V2 x (y - playerSpeed * if isFocussed then 0.5 else 1))

handleInput :: Event -> System' Event
handleInput event@(EventKey k Down _ _) = do
  global $~ \(Keys keys) -> Keys $ Set.insert (normalizeInput k) keys
  return event
handleInput event@(EventKey k Up _ _) = do
  global $~ \(Keys keys) -> Keys $ Set.delete (normalizeInput k) keys
  return event
handleInput event = return event

normalizeInput :: Key -> Key
normalizeInput k = case k of
  Char c -> Char $ toLower c
  key    -> key

handleEvent :: Event -> System' ()
handleEvent (EventKey (SpecialKey KeyLeft) Down _ _) =
  cmap $ \Player -> MovingLeft

handleEvent (EventKey (SpecialKey KeyLeft) Up _ _) =
  cmap $ \Player -> Not @MovingLeft

handleEvent (EventKey (SpecialKey KeyRight) Down _ _) =
  cmap $ \Player -> MovingRight

handleEvent (EventKey (SpecialKey KeyRight) Up _ _) =
  cmap $ \Player -> Not @MovingRight

handleEvent (EventKey (SpecialKey KeyUp) Down _ _) = cmap $ \Player -> MovingUp

handleEvent (EventKey (SpecialKey KeyUp) Up _ _) =
  cmap $ \Player -> (Not @MovingUp, Not @Jumping)

handleEvent (EventKey (SpecialKey KeyDown) Down _ _) =
  cmap $ \Player -> MovingDown

handleEvent (EventKey (SpecialKey KeyDown) Up _ _) =
  cmap $ \Player -> Not @MovingDown

handleEvent (EventKey (SpecialKey KeyShiftL) Down _ _) =
  cmap $ \Player -> Focus

handleEvent (EventKey (SpecialKey KeyShiftL) Up _ _) =
  cmap $ \Player -> Not @Focus

handleEvent (EventKey (SpecialKey KeyEsc) Down _ _) = liftIO exitSuccess

handleEvent (EventKey c                   Down _ _) = case normalizeInput c of
  Char 'x' -> do
    cmap $ \Player -> IsShooting 0 R
    cmap $ \(Player, IsShooting cooldown _) -> IsShooting cooldown R
  Char 'z' -> do
    cmap $ \Player -> IsShooting 0 L
    cmap $ \(Player, IsShooting cooldown _) -> IsShooting cooldown L
  _ -> return ()

handleEvent (EventKey c Up _ _) = case normalizeInput c of
  Char 'x' -> cmapM_ $ \(Player, IsShooting timeout _, Keys keys, e) ->
    if Set.member (normalizeInput (Char 'z')) keys
      then set e (IsShooting timeout L)
      else destroy e (Proxy @IsShooting)
  Char 'z' -> cmapM_ $ \(Player, IsShooting timeout _, Keys keys, e) ->
    if Set.member (normalizeInput (Char 'x')) keys
      then set e (IsShooting timeout R)
      else destroy e (Proxy @IsShooting)
  _ -> return ()

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
  enemyBullets <- foldDraw $ \(EnemyBullet pic size _, pos) ->
    translate' pos . color white . scale size size $ pic
  enemyBulletRanges <- foldDraw $ \(EnemyBullet{}, pos) ->
    translate' pos . color (greyN 0.1) $ circleSolid dangerZoneRange
  platforms <- foldDraw $ \(Platform, pos, Hitbox (V2 w h) _) ->
    translate' pos . color white . scale w h $ box
  -- hitboxes <- foldDraw $ \(Hitbox (V2 w h) offset, Position pos) ->
  --   translate' (Position $ pos + offset) . color yellow . scale w h $ box

  particles <- foldDraw $ \(Particle _, Velocity (V2 vx vy), pos) ->
    translate' pos . color white $ Line [(0, 0), (vx / 10, vy / 10)]

  Score s <- get global
  let score =
        color white
          .  translate' (Position scorePos)
          .  scale 0.1 0.1
          .  Text
          $  "Score: "
          ++ show (round s :: Integer)

  HighScore hs <- get global
  let highScore =
        color white
          .  translate' (Position highScorePos)
          .  scale 0.1 0.1
          .  Text
          $  "High Score: "
          ++ show (round hs :: Integer)

  return
    $  enemyBulletRanges
    <> player
    <> enemys
    <> bullets
    <> score
    <> highScore
    <> particles
    <> platforms
    <> enemyBullets
    -- <> hitboxes
