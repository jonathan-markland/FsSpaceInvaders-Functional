module GameDrawing

open GamePlayInternal
open GamePlayTypes
open ScreenLayout
open DrawingCommands
open Fonts
open ScoreHiScore



let RenderGameWorld render (gameWorld:GameWorld) =

    render (GameplayBackground)

    gameWorld.Motherships |> List.iter 
        (fun motherShip -> 
            render (
                DrawMothership(
                    motherShip.MothershipExtents.LeftW,
                    motherShip.MothershipExtents.TopW)))

    gameWorld.Invaders |> List.iter
        (fun invader -> 
            render (
                DrawInvader(
                    invader.InvaderExtents.LeftW,
                    invader.InvaderExtents.TopW,
                    invader.DogTag)))

    if gameWorld.PlayEndedYet |> Option.isNone then

        let theShip = gameWorld.Ship
        let shipL = theShip.ShipExtents.LeftW
        let shipT = theShip.ShipExtents.TopW

        render (DrawShip(shipL, shipT))

        match theShip.WeaponReloadStartTimeOpt with
            | Some(_) -> ()
            | None    -> render (DrawBullet (BulletPositionOnTopOfShip theShip.ShipExtents))

    gameWorld.Bullets |> List.iter
        (fun bullet -> 
            render (
                DrawBullet(
                    bullet.BulletExtents.LeftW,
                    bullet.BulletExtents.TopW)))

    gameWorld.Bombs |> List.iter
        (fun bomb -> 
            render (
                DrawBomb(
                    bomb.BombExtents.LeftW,
                    bomb.BombExtents.TopW)))

    gameWorld.Explosions |> List.iter
        (fun explosion ->
            render (DrawExplosion(explosion)))

    let text x top message alignmentH alignmentV =
        render (DrawText (x, top, message, alignmentH, alignmentV))

    let number x top (value:uint32) alignmentH alignmentV =
        let s = value.ToString()
        render (DrawText (x, top, s, alignmentH, alignmentV))

    text   HeadingScoreX   ScoreboardTitlesTopY "SCORE"   LeftAlign   TopAlign
    text   HeadingHiScoreX ScoreboardTitlesTopY "HISCORE" CentreAlign TopAlign
    text   HeadingLevelX   ScoreboardTitlesTopY "LEVEL"   CentreAlign TopAlign
    text   HeadingLivesX   ScoreboardTitlesTopY "LIVES"   RightAlign  TopAlign

    let { Score=score ; HiScore=hiScore } = gameWorld.ScoreAndHiScore

    number HeadingScoreX   ScoreboardValuesTopY score   LeftAlign   TopAlign 
    number HeadingHiScoreX ScoreboardValuesTopY hiScore CentreAlign TopAlign
    number HeadingLevelX   ScoreboardValuesTopY gameWorld.LevelAndLives.Level   CentreAlign TopAlign
    number HeadingLivesX   ScoreboardValuesTopY gameWorld.LevelAndLives.Lives   RightAlign TopAlign


