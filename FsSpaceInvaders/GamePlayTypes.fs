module GamePlayTypes

open Geometry
open Mechanics
open Dimensions
open ScoreHiScore


type LevelAndLives =
    {
        Level:             uint32
        Lives:             uint32
    }


type Ship = 
    {
        ShipExtents: RectangleW

        /// If None, the ship's weapon is ready.
        /// If Some() it gives the tick count at which
        /// the reload penalty began.
        WeaponReloadStartTimeOpt: TickCount option
    }


type InvaderColour = RedInvader | BlueInvader

type DogTag = DogTag of int


/// Record to represent invader instance, created
/// only when the invader instance appears at the
/// start of the game level.
type Invader =
    {
        /// Used to identify instance, calculate
        /// its relative position in the formation,
        /// and calculate a colour.
        DogTag:  DogTag

        InvaderExtents: RectangleW
    }

let AreaOfInvader i = i.InvaderExtents
let DogTagOfInvader i = i.DogTag


/// Record to represent mothership instance, created
/// only when the mothership instance appears.
type Mothership =
    {
        MothershipExtents: RectangleW
    }

let AreaOfMothership m = m.MothershipExtents


/// Record to represent bullet instance, created
/// only when the bullet instance appears.
type Bullet =
    {
        BulletExtents: RectangleW
    }

let AreaOfBullet b = b.BulletExtents

let NewBulletAt (x,y) =
    { 
        BulletExtents = { LeftW=x ; TopW=y ; RightW=x+BulletWidth ; BottomW=y+BulletHeight }
    }


/// Record to represent bomb instance, created
/// only when the bomb instance appears.
type Bomb =
    {
        BombExtents: RectangleW
    }

let AreaOfBomb b = b.BombExtents

let NewBombAt (x,y) =
    { 
        BombExtents = { LeftW=x ; TopW=y ; RightW=x+BombWidth ; BottomW=y+BombHeight }
    }


/// Record to represent explosion instance, created
/// only when the explosion instance appears.
/// TimeForWholeExplosion gives the duration.
/// Explosions can be different sizes.
type Explosion =
    {
        ExplosionExtents: RectangleW
        StartTime:        TickCount
    }

let AreaOfExplosion b = b.ExplosionExtents

let NewExplosionAt extentsRectangle startTime =
    { 
        ExplosionExtents = extentsRectangle
        StartTime        = startTime
    }



/// The Game world state during gameplay.
type GameWorld =
    {
        GameStartTime:      TickCount
        ScoreAndHiScore:    ScoreAndHiScore
        LevelAndLives:      LevelAndLives
        Motherships:        Mothership list  // anticipated max one for now, but type-similarity to Invaders allows some uniform handling.
        Invaders:           Invader list
        Bullets:            Bullet list
        Bombs:              Bomb list
        Explosions:         Explosion list

        /// The Ship is 'None' when it has been destroyed.
        Ship:               Ship option
    }



type Screen =
    | WelcomeScreen                    of hiScore:uint32
    | GamePlayScreen                   of GameWorld
    | LifeOverResidualAnimationScreen  of GameWorld * screenEntryTime:TickCount
    | LifeOverCardScreen               of GameWorld
    | NextLevelResidualAnimationScreen of GameWorld * screenEntryTime:TickCount
    | NextLevelCardScreen              of GameWorld
    | GameOverResidualAnimationScreen  of GameWorld * screenEntryTime:TickCount
    | GameOverCardScreen               of GameWorld

