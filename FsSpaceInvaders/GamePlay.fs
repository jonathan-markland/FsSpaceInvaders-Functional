module GamePlay

open InputEventData
open GamePlayTypes
open GamePlayInternal
open Rules
open Geometry
open Mechanics
open ScreenLayout
open Dimensions
open Scoring
open Rates
open ScoreHiScore

// TODO:  Speed up as number of invaders reduces.
// TODO:  Invader step downwards should be larger.  Timing of their advance needs revisiting.
// TODO:  How could we change the game each level?



let NewInvaderPack () =

    let invaderHorizSpacing = ScreenWidth / (InvadersPerRow + 1)    // NB: Intentionally is integer division, so will truncate.
    let invaderHorizSpan    = invaderHorizSpacing * (InvadersPerRow - 1)   // NB: We centre THIS span, so it looks nice even if division truncated.
    let invaderLeftSide     = (ScreenWidth - invaderHorizSpan) / 2

    let NewInvader x y =

        let InitialPositionCentreForInvader x y =
            { 
                xw = (x-1) * invaderHorizSpacing + invaderLeftSide
                yw = InvadersTopY + (y-1) * (InvaderHeight + InvaderVSpacing) 
            }

        {
            DogTag = DogTag(y * InvadersPerRow + x)
            InvaderExtents = (InitialPositionCentreForInvader x y) |> RectangleCenteredAboutPoint InvaderWidth InvaderHeight
        }

    [for y in 1..InvaderRowsCount do
        for x in 1..InvadersPerRow do
            NewInvader x y]



let ShipInLevelStartPosition () =

    {
        WeaponReloadStartTimeOpt = None

        ShipExtents =
            let x = ScreenCentreX - (ShipWidth / 2)
            let y = ShipTopY
            { 
                LeftW   = x
                TopW    = y
                RightW  = x + ShipWidth
                BottomW = y + ShipHeight
            } 
    }



/// Create a fresh game world for a new game.
let NewGameWorld hiScore (timeNow:TickCount) : GameWorld =

    {
        GameStartTime = timeNow

        LevelAndLives =
            {
                Level   = 1u
                Lives   = 3u
            }

        ScoreAndHiScore = { Score=0u ; HiScore=hiScore }
        Motherships  = []
        Invaders     = NewInvaderPack ()
        Bullets      = []
        Bombs        = []
        Explosions   = []
        Ship         = Some(ShipInLevelStartPosition ())
    }



/// Given a world where the player has just lost a life,
/// return a new world for the next life.
let NextLifeGameWorld (timeNow:TickCount) (outgoing:GameWorld) : GameWorld =

    let oldStats = outgoing.LevelAndLives

    {
        GameStartTime   = timeNow
        LevelAndLives   = { oldStats with Lives = oldStats.Lives - 1u }
        ScoreAndHiScore = outgoing.ScoreAndHiScore
        Motherships     = outgoing.Motherships
        Invaders        = outgoing.Invaders
        Bullets         = []
        Bombs           = []
        Explosions      = []
        Ship            = Some(ShipInLevelStartPosition ())
    }



/// Given a world where the player has just won the level,
/// return a new world for the next level.
let NextLevelGameWorld (timeNow:TickCount) (outgoing:GameWorld) : GameWorld =

    let oldStats   = outgoing.LevelAndLives

    {
        GameStartTime   = timeNow
        LevelAndLives   = { Lives = oldStats.Lives + 1u ; Level = oldStats.Level + 1u }
        ScoreAndHiScore = outgoing.ScoreAndHiScore |> IncrementScoreBy ScoreForNextLevel
        Motherships     = []
        Invaders        = NewInvaderPack ()
        Bullets         = []
        Bombs           = []
        Explosions      = []
        Ship            = Some(ShipInLevelStartPosition ())
    }



type FrameResult = 
    | GameContinuing of GameWorld 
    | PlayerWon of GameWorld
    | PlayerLost of GameWorld



let CalculateNextFrameState (oldWorld:GameWorld) (input:InputEventData) (timeNow:TickCount) =

    let elapsedTime = timeNow --- oldWorld.GameStartTime

    let ship, bullets =
        match oldWorld.Ship with
            
            | None ->
                None, oldWorld.Bullets
            
            | Some(ship) ->

                let newShipExtents =
                    MoveShip ship.ShipExtents input

                let bullets, reloadPenalty =
                    ConsiderBulletFiring 
                        oldWorld.Bullets 
                        ship
                        newShipExtents
                        timeNow
                        input

                Some({
                    ShipExtents = newShipExtents
                    WeaponReloadStartTimeOpt = reloadPenalty
                }),
                bullets

    let bombs = 
        ConsiderDroppingBombs oldWorld.Bombs oldWorld.Invaders timeNow elapsedTime 
            |> MoveBombs

    let bullets =
        MoveBullets bullets

    let bullets, invaders, explosions, scoreIncreaseFromInvaders = 
        ConsiderShotInvaders bullets oldWorld.Invaders oldWorld.Explosions timeNow
            
    let bullets, motherships, explosions, scoreIncreaseFromMotherships =
        ConsiderShotMothership bullets oldWorld.Motherships explosions timeNow
            
    let invaders = 
        MoveInvaders invaders elapsedTime
    
    let motherships = 
        motherships |> MoveMotherships |> ConsiderIntroducingMothership elapsedTime
    
    let explosions = 
        ConsiderRemovingExplosions explosions timeNow

    let levelOver =
        match ship with 
            | None -> true
            | Some(ship) -> LevelOver ship.ShipExtents invaders bombs

    let explosions, newShip =
        if levelOver then
            match ship with
                | None -> explosions, ship
                | Some(ship) -> (ExplodeTheShip ship explosions timeNow, None)
        else
            explosions, ship

    let newWorld =
        {
            oldWorld with
                ScoreAndHiScore = oldWorld.ScoreAndHiScore |> IncrementScoreBy (scoreIncreaseFromInvaders + scoreIncreaseFromMotherships) 
                Motherships     = motherships
                Invaders        = invaders
                Bullets         = bullets
                Bombs           = bombs
                Explosions      = explosions
                Ship            = newShip
        }

    if levelOver then
        PlayerLost(newWorld)
    else if NoInvadersLeft invaders then 
        PlayerWon(newWorld)
    else
        GameContinuing(newWorld)



type AnimatedSequenceResult<'T> =
    | AnimationContinuing of 'T
    | AnimationFinished of 'T
                    


/// This is used for the short sequence after a life loss
/// or a level win.  This allows animations to continue on the
/// gameplay screen for a while before clearing.
let CalculateNextPlaySuspendedState (oldWorld:GameWorld) (timeNow:TickCount) endedAt =

    let elapsedInEndState = timeNow --- endedAt

    if elapsedInEndState < TimeForEndState then

        let newWorld = 
            match CalculateNextFrameState oldWorld InputsWhereNothingIsPressed timeNow with
                | PlayerLost(w) -> w
                | PlayerWon(w) -> w
                | GameContinuing(w) -> w

        AnimationContinuing(newWorld)

    else

        AnimationFinished(oldWorld)

