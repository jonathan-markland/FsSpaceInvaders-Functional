module GamePlayInternal

open InputEventData
open Algorithm
open GamePlayTypes
open Geometry
open Mechanics
open ScreenLayout
open Dimensions
open Scoring
open Rates


let BulletPositionOnTopOfShip shipExtents =

    let shipL = shipExtents.LeftW
    let shipT = shipExtents.TopW

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






let RandomInvader invadersList timeNow =  // TODO: This exhibits really bad gameplay

    match invadersList with
        | [] ->
            None
        | _  ->
            let countLeft = invadersList |> List.length
            let (TickCount(ticks)) = timeNow
            let selectedIndex = int (ticks % uint32 countLeft)
            Some(invadersList |> List.item selectedIndex)

let MoveShip oldShipExtents (input:InputEventData) =

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

let ConsiderBulletFiring oldBullets (ship:Ship) shipExtents timeNow input =

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
            let newBullet = NewBulletFiredFromCentrallyAbove shipExtents
            let updatedBulletList = newBullet :: oldBullets
            updatedBulletList, Some(timeNow)
        else
            oldBullets, reloadPenalty

    ship.WeaponReloadStartTimeOpt |> ConsiderReloadPenalty |> CheckFireButton

let ConsiderDroppingBombs oldBombs invaders timeNow elapsedTime =

    Every TimeForNewBombCheck elapsedTime oldBombs (fun state ->
        let firingInvader = RandomInvader invaders timeNow
        match firingInvader with
            | None -> 
                state
            | Some(firingInvader) -> 
                let newBomb = NewBombPositionedCentrallyUnder (firingInvader |> AreaOfInvader)
                let updateBombsList = newBomb :: oldBombs
                updateBombsList
    )

let MoveBullets oldBulletsList =

    let ApplyUpwardMovementToBullet b =
        { b with BulletExtents = b.BulletExtents |> RectangleShuntedBy 0<wu> -BulletStep }

    let WhereBulletStillBelowTopmostPosition bullet =
        bullet.BulletExtents.TopW > BulletEndY

    let bulletsStillInPlay = 
        oldBulletsList |> List.filter WhereBulletStillBelowTopmostPosition   // TODO: optimise for case where all are on screen still

    bulletsStillInPlay |> List.map ApplyUpwardMovementToBullet

let MoveBombs oldBombsList =

    let ApplyDownwardMovementToBomb b =
        { b with BombExtents = b.BombExtents |> RectangleShuntedBy 0<wu> BombStep }

    let WhereBombStillAboveFloorPosition bomb =
        bomb.BombExtents.BottomW < BombFloorY

    let bombsStillInPlay = 
        oldBombsList |> List.filter WhereBombStillAboveFloorPosition   // TODO: optimise for case where all are on screen still

    bombsStillInPlay |> List.map ApplyDownwardMovementToBomb

let WithAdditionalExplosionsFor listOfThings areaOfThing preExistingExplosions timeNow =

    preExistingExplosions |> List.append

        (listOfThings |> List.map (fun t ->
            {
                ExplosionExtents = areaOfThing t
                StartTime        = timeNow
            }
        ))

let ConsiderShotInvaders oldBullets oldInvaders oldExplosions timeNow =

    let deadBullets,deadInvaders = 
        CollisionsBetweenLists 
            (oldBullets  |> WithAreasObtainedBy AreaOfBullet)
            (oldInvaders |> WithAreasObtainedBy AreaOfInvader)

    let scoreIncrease      = uint32 (List.length deadInvaders) * ScoreForKillingInvader
    let survivingInvaders  = oldInvaders |> List.filter (NotInList deadInvaders DogTagOfInvader)  // TODO: Prepare to return same list favouring no removals
    let survivingBullets   = oldBullets |> List.filter (NotInList deadBullets AreaOfBullet)  // TODO: Prepare to return same list favouring no removals
    let newExplosionsState = WithAdditionalExplosionsFor deadInvaders AreaOfInvader oldExplosions timeNow

    survivingBullets, survivingInvaders, newExplosionsState, scoreIncrease

let ConsiderShotMothership oldBullets oldMotherships oldExplosions timeNow =

    // TODO: Performance optimise:  Don't do any of this if no motherships (a common case)

    let deadBullets,deadMotherships = 
        CollisionsBetweenLists 
            (oldBullets |> WithAreasObtainedBy AreaOfBullet)
            (oldMotherships |> WithAreasObtainedBy AreaOfMothership)

    let scoreIncrease        = uint32 (List.length deadMotherships) * ScoreForKillingMothership
    let survivingMotherships = oldMotherships |> List.filter (NotInList deadMotherships AreaOfMothership)  // TODO: Prepare to return same list favouring no removals
    let survivingBullets     = oldBullets |> List.filter (NotInList deadBullets AreaOfBullet)  // TODO: Prepare to return same list favouring no removals
    let newExplosionsState   = WithAdditionalExplosionsFor deadMotherships AreaOfMothership oldExplosions timeNow

    survivingBullets, survivingMotherships, newExplosionsState, scoreIncrease

let ConsiderRemovingExplosions oldExplosions timeNow =

    oldExplosions |> List.filter (fun e ->     // TODO: Prepare to return same list favouring no removals
        let elapsedSinceExplosionStarted = timeNow --- e.StartTime
        elapsedSinceExplosionStarted < TimeForWholeExplosion)

let MoveInvaders oldInvaders elapsedTime =
 
    let (TickSpan(ticks)) = elapsedTime

    // TODO: This accumulates strangely, resulting in invaders tending off-piste,
    //       particularly around life loss sequences.

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

let ConsiderIntroducingMothership oldMotherships elapsedTime =

    Every TimeForMothershipCheck elapsedTime oldMotherships (fun oldMotherships ->
        let x = MothershipCentreStartX - (MothershipWidth / 2)
        let newMothership = { MothershipExtents = { LeftW=x ; TopW=MotherShipTopY ; RightW=x+MothershipWidth ; BottomW=MotherShipTopY+MothershipHeight } }
        newMothership :: oldMotherships
    )

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

let ExplodeTheShip oldShip oldExplosions timeNow =

    let shipExplosion = 
        {
            ExplosionExtents = oldShip.ShipExtents
            StartTime = timeNow
        }

    shipExplosion :: oldExplosions

let NoInvadersLeft (invaders:Invader list) =

    invaders.IsEmpty

let LevelOver shipExtents invaders bombs =

    let InvaderAtLowestLevel invaders =

        let atLowestLevel invader = invader.InvaderExtents.BottomW >= ShipTopY
        invaders |> List.exists (fun invader -> invader |> atLowestLevel)

    let ShipCollidedWithInvader shipExtents invaders =

        let collidedWithShip invader = invader.InvaderExtents |> RectangleIntersects shipExtents
        invaders |> List.exists (fun invader -> invader |> collidedWithShip)

    let ShipCollidedWithBomb shipExtents bombs =

        let collidedWithShip bomb = bomb.BombExtents |> RectangleIntersects shipExtents
        bombs |> List.exists (fun bomb -> bomb |> collidedWithShip)

    InvaderAtLowestLevel invaders
        || ShipCollidedWithInvader shipExtents invaders
        || ShipCollidedWithBomb shipExtents bombs

