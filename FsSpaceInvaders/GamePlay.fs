module GamePlay

open Algorithm
open InputEventData
open GamePlayTypes
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

        PlayStats =
            {
                Level   = 1u
                ScoreAndHiScore = { Score=0u ; HiScore=hiScore }
                Lives   = 3u
            }

        Motherships  = []
        Invaders     = NewInvaderPack ()
        Bullets      = []
        Bombs        = []
        Explosions   = []
        Ship         = ShipInLevelStartPosition ()
        PlayEndedYet = None
    }



/// Given a world where the player has just lost a life,
/// return a new world for the next life.
let NextLifeGameWorld (timeNow:TickCount) (outgoing:GameWorld) : GameWorld =

    let oldStats = outgoing.PlayStats

    {
        GameStartTime = timeNow
        PlayStats     = { oldStats with Lives = oldStats.Lives - 1u }
        Motherships   = outgoing.Motherships
        Invaders      = outgoing.Invaders
        Bullets       = []
        Bombs         = []
        Explosions    = []
        Ship          = outgoing.Ship
        PlayEndedYet  = None
    }



/// Given a world where the player has just won the level,
/// return a new world for the next level.
let NextLevelGameWorld (timeNow:TickCount) (outgoing:GameWorld) : GameWorld =

    let oldStats   = outgoing.PlayStats
    let newScoring = oldStats.ScoreAndHiScore |> IncrementScoreBy ScoreForNextLevel

    {
        GameStartTime = timeNow
        PlayStats     = { oldStats with Lives = oldStats.Lives + 1u ; Level = oldStats.Level + 1u ; ScoreAndHiScore = newScoring }
        Motherships   = []
        Invaders      = NewInvaderPack ()
        Bullets       = []
        Bombs         = []
        Explosions    = []
        Ship          = ShipInLevelStartPosition ()
        PlayEndedYet  = None
    }



type FrameResult = 
    | GameContinuing of GameWorld 
    | PlayerWon of GameWorld
    | PlayerLost of GameWorld



let BulletPositionOnTopOfShip theShip =

    let shipL = theShip.ShipExtents.LeftW
    let shipT = theShip.ShipExtents.TopW

    let bleft = shipL + ((ShipWidth - BulletWidth)/2)
    let btop  = shipT - BulletHeight

    (bleft,btop)



let NewBulletFiredFromCentrallyAbove theShip =

    let x,y = BulletPositionOnTopOfShip theShip

    {
        BulletExtents =
            {
                LeftW    = x
                RightW   = x + BulletWidth
                TopW     = y
                BottomW  = y + BulletHeight
            }
    }



let NewBombPositionedCentrallyUnder someRectangle =

    let leftSide = (HorizontalCentreOf someRectangle) - (BombWidth / 2)
    let topY    = someRectangle.BottomW

    {
        BombExtents =
            {
                LeftW    = leftSide
                RightW   = leftSide + BombWidth
                TopW     = topY 
                BottomW  = topY + BombHeight
            }
    }




let CalculateNextFrameState (oldWorld:GameWorld) (input:InputEventData) (timeNow:TickCount) =

    let elapsedTime = timeNow --- oldWorld.GameStartTime

    let RandomInvader invadersList =  // TODO: This exhibits really bad gameplay

        match invadersList with
            | [] ->
                None
            | _  ->
                let countLeft = invadersList |> List.length
                let (TickCount(ticks)) = timeNow
                let selectedIndex = int (ticks % uint32 countLeft)
                Some(invadersList |> List.item selectedIndex)

    let MoveShip oldShipExtents =

        let movementStep =  
            if input.LeftHeld && input.RightHeld then
                0<wu>
            elif input.LeftHeld && (HorizontalCentreOf oldShipExtents) > ShipCentreLeftmostX then
                -ShipMovementStep
            elif input.RightHeld && (HorizontalCentreOf oldShipExtents) < ShipCentreRightmostX then
                ShipMovementStep
            else
                0<wu>

        oldShipExtents |> RectangleShuntedBy movementStep 0<wu>
        // TODO: assign to world.Ship.ShipExtents

    let ConsiderBulletFiring oldBullets oldWeaponReload =

        let ConsiderReloadPenalty oldWeaponReload =

            match oldWeaponReload with
                | Some(startTime) -> 
                    if (timeNow --- startTime) >= TimeForReloadShipWeapon then
                        None
                    else
                        oldWeaponReload
                | None ->
                    None

        let CheckFireButton reloadPenalty =

            if input.FireJustPressed && reloadPenalty |> Option.isNone then
                let newBullet = NewBulletFiredFromCentrallyAbove oldWorld.Ship
                let updatedBulletList = newBullet :: oldBullets
                updatedBulletList, Some(timeNow)
            else
                oldBullets, reloadPenalty

        oldWeaponReload |> ConsiderReloadPenalty |> CheckFireButton

            // TODO:  assign to  world.Bullets <- updatedBulletList
            // TODO:  assign to  world.Ship.WeaponReloadStartTimeOpt <- Some(timeNow)

    let ConsiderDroppingBombs oldBombs invaders =

        Every TimeForNewBombCheck elapsedTime oldBombs (fun state ->
            let firingInvader = RandomInvader invaders
            match firingInvader with
                | None -> 
                    state
                | Some(firingInvader) -> 
                    let newBomb = NewBombPositionedCentrallyUnder (firingInvader |> AreaOfInvader)
                    let updateBombsList = newBomb :: oldBombs
                    updateBombsList
        )
        // TODO: assign to world.Bombs

    let MoveBullets oldBulletsList =

        let ApplyUpwardMovementToBullet b =
            { b with BulletExtents = b.BulletExtents |> RectangleShuntedBy 0<wu> -BulletStep }

        let WhereBulletStillBelowTopmostPosition bullet =
            bullet.BulletExtents.TopW > BulletEndY

        let bulletsStillInPlay = 
            oldBulletsList |> List.filter WhereBulletStillBelowTopmostPosition   // TODO: optimise for case where all are on screen still

        bulletsStillInPlay |> List.map ApplyUpwardMovementToBullet

        // TODO: Assign to  world.Bullets <- bulletsStillInPlay

    let MoveBombs oldBombsList =

        let ApplyDownwardMovementToBomb b =
            { b with BombExtents = b.BombExtents |> RectangleShuntedBy 0<wu> BombStep }

        let WhereBombStillAboveFloorPosition bomb =
            bomb.BombExtents.BottomW < BombFloorY

        let bombsStillInPlay = 
            oldBombsList |> List.filter WhereBombStillAboveFloorPosition   // TODO: optimise for case where all are on screen still

        bombsStillInPlay |> List.map ApplyDownwardMovementToBomb

        // TODO: world.Bombs <- bombsStillInPlay

    let WithAdditionalExplosionsFor listOfThings areaOfThing preExistingExplosions =

        preExistingExplosions |> List.append

            (listOfThings |> List.map (fun t ->
                {
                    ExplosionExtents = areaOfThing t
                    StartTime        = timeNow
                }
            ))

    let ConsiderShotInvaders oldBullets oldInvaders oldExplosions =

        let deadBullets,deadInvaders = 
            CollisionsBetweenLists 
                (oldBullets  |> WithAreasObtainedBy AreaOfBullet)
                (oldInvaders |> WithAreasObtainedBy AreaOfInvader)

        let scoreIncrease      = uint32 (List.length deadInvaders) * ScoreForKillingInvader
        let survivingInvaders  = oldInvaders |> List.filter (NotInList deadInvaders DogTagOfInvader)  // TODO: Prepare to return same list favouring no removals
        let survivingBullets   = oldBullets |> List.filter (NotInList deadBullets AreaOfBullet)  // TODO: Prepare to return same list favouring no removals
        let newExplosionsState = oldExplosions |> WithAdditionalExplosionsFor deadInvaders AreaOfInvader 

        survivingBullets, survivingInvaders, newExplosionsState, scoreIncrease

        // TODO:  world.Bullets <- survingBullets
        // TODO:  world.Invaders <- survingInvaders
        // TODO:  world.Explosions <- newExplosionsState
        // TODO:  IncreaseScoreBy scoreIncrease

    let ConsiderShotMothership oldBullets oldMotherships oldExplosions =

        // TODO: Performance optimise:  Don't do any of this if no motherships (a common case)

        let deadBullets,deadMotherships = 
            CollisionsBetweenLists 
                (oldBullets |> WithAreasObtainedBy AreaOfBullet)
                (oldMotherships |> WithAreasObtainedBy AreaOfMothership)

        let scoreIncrease        = uint32 (List.length deadMotherships) * ScoreForKillingMothership
        let survivingMotherships = oldMotherships |> List.filter (NotInList deadMotherships AreaOfMothership)  // TODO: Prepare to return same list favouring no removals
        let survivingBullets     = oldBullets |> List.filter (NotInList deadBullets AreaOfBullet)  // TODO: Prepare to return same list favouring no removals
        let newExplosionsState   = oldExplosions |> WithAdditionalExplosionsFor deadMotherships AreaOfMothership

        survivingBullets, survivingMotherships, newExplosionsState, scoreIncrease

        // TODO: world.Bullets <- survingBullets
        // TODO: world.Motherships <- survingMotherships
        // TODO: world.Explosions <- sumTotalExplosions
        // TODO: IncreaseScoreBy scoreIncrease

    let ConsiderRemovingExplosions oldExplosions =

        let survivingExplosions = oldExplosions |> List.filter (fun e ->     // TODO: Prepare to return same list favouring no removals
            let elapsedSinceExplosionStarted = timeNow --- e.StartTime
            elapsedSinceExplosionStarted < TimeForWholeExplosion)

        survivingExplosions
        // TOdo:  world.Explosions <- survivingExplosions

    let MoveInvaders oldInvaders =
 
        let (TickSpan(ticks)) = elapsedTime

        let dx = if (ticks % (TimeForInvaderWiggle * 2u)) >= TimeForInvaderWiggle then InvaderWiggleStep  else -InvaderWiggleStep
        let dy = if (ticks % TimeForInvaderAdvance) = 0u then InvaderAdvanceStep else 0<wu>

        oldInvaders |> List.map (fun invader ->
            let old = invader.InvaderExtents
            let newExtents =
                {
                    LeftW     = old.LeftW   + dx
                    TopW      = old.TopW    + dy
                    RightW    = old.RightW  + dx
                    BottomW   = old.BottomW + dy
                }
            { invader with InvaderExtents = newExtents } 
        )

    let ConsiderIntroducingMothership oldMotherships =

        Every TimeForMothershipCheck elapsedTime oldMotherships (fun oldMotherships ->
            let x = MothershipCentreStartX - (MothershipWidth / 2)
            let newMothership = { MothershipExtents = { LeftW=x ; TopW=MotherShipTopY ; RightW=x+MothershipWidth ; BottomW=MotherShipTopY+MothershipHeight } }
            newMothership :: oldMotherships
        )
        // TODO: assign to world.Motherships

    let MoveMotherships oldMotherships =

        let dx = MothershipStep

        let movedMotherships = oldMotherships |> List.map (fun mothership ->
            let old = mothership.MothershipExtents
            { mothership with MothershipExtents = { old with LeftW = old.LeftW + dx ; RightW = old.RightW + dx } }
            )

        let atFinishPosition mothership =
            mothership.MothershipExtents.RightW = (MothershipCentreEndX + MothershipWidth / 2)

        if movedMotherships |> List.exists atFinishPosition then
            let survivingMotherships =
                movedMotherships |> List.filter (fun mothership -> not (mothership |> atFinishPosition))
            survivingMotherships
        else
            movedMotherships

        // TODO: assign to world.Motherships <- 

    let NoInvadersLeft (invaders:Invader list) =
    
        invaders.IsEmpty
    
    let InvaderAtLowestLevel invaders =

        let atLowestLevel invader = invader.InvaderExtents.BottomW >= ShipTopY
        invaders |> List.exists (fun invader -> invader |> atLowestLevel)

    let ShipCollidedWithInvader shipExtents invaders =

        let collidedWithShip invader = invader.InvaderExtents |> RectangleIntersects shipExtents
        invaders |> List.exists (fun invader -> invader |> collidedWithShip)

    let ShipCollidedWithBomb shipExtents bombs =

        let collidedWithShip bomb = bomb.BombExtents |> RectangleIntersects shipExtents
        bombs |> List.exists (fun bomb -> bomb |> collidedWithShip)

    let ExplodeTheShip oldShip oldExplosions =

        let shipExplosion = 
            {
                ExplosionExtents = oldShip.ShipExtents
                StartTime = timeNow
            }

        let newExplosionsList =
            shipExplosion :: oldExplosions

        newExplosionsList
        // TODO: update  world.Explosions <- newExplosionsList

    let LevelOver shipExtents invaders bombs =
        InvaderAtLowestLevel invaders
        || ShipCollidedWithInvader shipExtents invaders
        || ShipCollidedWithBomb shipExtents bombs

    match oldWorld.PlayEndedYet with

        | None ->
            let newShipExtents = MoveShip oldWorld.Ship.ShipExtents
            let newBullets, newReloadPenalty = ConsiderBulletFiring oldWorld.Bullets oldWorld.Ship.WeaponReloadStartTimeOpt
            let newBombs     = ConsiderDroppingBombs oldWorld.Bombs oldWorld.Invaders
            let movedBullets = MoveBullets newBullets
            let movedBombs   = MoveBombs newBombs
            let survivingBullets, survivingInvaders, newExplosionsState, scoreIncreaseFromInvaders = 
                ConsiderShotInvaders movedBullets oldWorld.Invaders oldWorld.Explosions
            let survivingBullets, survivingMotherships, newExplosionsState, scoreIncreaseFromMotherships =
                ConsiderShotMothership survivingBullets oldWorld.Motherships newExplosionsState
            let movedInvaders     = MoveInvaders survivingInvaders
            let movedMotherships  = MoveMotherships survivingMotherships
            let newMotherships    = ConsiderIntroducingMothership movedMotherships
            let ongoingExplosions = ConsiderRemovingExplosions newExplosionsState
            
            let newShip =
                {
                    ShipExtents = newShipExtents
                    WeaponReloadStartTimeOpt = newReloadPenalty
                }

            let newPlayEndedYet, ongoingExplosions =
                if NoInvadersLeft movedInvaders then 
                    Some(timeNow, EndBecauseWon), ongoingExplosions
                else if LevelOver newShip.ShipExtents movedInvaders movedBombs then
                    Some(timeNow, EndBecauseLost), (ExplodeTheShip newShip ongoingExplosions)
                else
                    None, ongoingExplosions

            let newWorld =
                {
                    oldWorld with
                        // TODO:  separate the score out from the other "GamePlayStats"
                        PlayStats      = { oldWorld.PlayStats with ScoreAndHiScore = oldWorld.PlayStats.ScoreAndHiScore |> IncrementScoreBy (scoreIncreaseFromInvaders + scoreIncreaseFromMotherships) } 
                        Motherships    = newMotherships
                        Invaders       = movedInvaders
                        Bullets        = survivingBullets
                        Bombs          = movedBombs
                        Explosions     = ongoingExplosions
                        Ship           = newShip
                        PlayEndedYet   = newPlayEndedYet
                }

            GameContinuing(newWorld)

        | Some(endedAt,reason) ->

            let elapsedInEndState = timeNow --- endedAt

            if elapsedInEndState < TimeForEndState then

                let movedBullets = MoveBullets oldWorld.Bullets
                let movedBombs = MoveBombs oldWorld.Bombs
                let survivingBullets, survivingInvaders, newExplosionsState, _ = 
                    ConsiderShotInvaders movedBullets oldWorld.Invaders oldWorld.Explosions
                let survivingBullets, survivingMotherships, newExplosionsState, _ =
                    ConsiderShotMothership survivingBullets oldWorld.Motherships newExplosionsState
                let movedInvaders = MoveInvaders survivingInvaders
                let movedMotherships = MoveMotherships survivingMotherships
                let ongoingExplosions = ConsiderRemovingExplosions newExplosionsState

                let newWorld = 
                    {
                        oldWorld with
                            Motherships    = movedMotherships
                            Invaders       = movedInvaders
                            Bullets        = survivingBullets
                            Bombs          = movedBombs
                            Explosions     = ongoingExplosions
                    }

                GameContinuing(newWorld)

            else
                match reason with
                    | EndBecauseWon  -> PlayerWon(oldWorld)
                    | EndBecauseLost -> PlayerLost(oldWorld)
                    
