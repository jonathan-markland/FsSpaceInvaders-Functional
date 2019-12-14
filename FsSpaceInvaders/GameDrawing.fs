﻿module GameDrawing

open GamePlayTypes
open ScreenLayout
open Dimensions
open DrawingCommands
open Fonts

let RenderGameWorld render (gameWorld:GameWorld) =

    let BulletPositionOnTopOfShip theShip =

        let shipL = theShip.ShipExtents.LeftW
        let shipT = theShip.ShipExtents.TopW

        let bleft = shipL + ((ShipWidth - BulletWidth)/2)
        let btop  = shipT - BulletHeight

        (bleft,btop)

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

    let theShip = gameWorld.Ship
    let shipL = theShip.ShipExtents.LeftW
    let shipT = theShip.ShipExtents.TopW

    render (DrawShip(shipL, shipT))

    match theShip.WeaponReloadStartTimeOpt with
        | Some(_) -> ()
        | None    -> render (DrawBullet (BulletPositionOnTopOfShip theShip))

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

    let text x top message alignment =
        render (DrawText (x, top, message, alignment))

    let number x top (value:int) alignment =
        let s = value.ToString()
        render (DrawText (x, top, s, alignment))

    let HeadingAlignmentScore   = LeftAlign
    let HeadingAlignmentHiScore = CentreAlign
    let HeadingAlignmentLevel   = CentreAlign
    let HeadingAlignmentLives   = RightAlign

    text   HeadingScoreX   ScoreboardTitlesTopY "SCORE"   HeadingAlignmentScore  
    text   HeadingHiScoreX ScoreboardTitlesTopY "HISCORE" HeadingAlignmentHiScore
    text   HeadingLevelX   ScoreboardTitlesTopY "LEVEL"   HeadingAlignmentLevel  
    text   HeadingLivesX   ScoreboardTitlesTopY "LIVES"   HeadingAlignmentLives  

    number HeadingScoreX   ScoreboardValuesTopY gameWorld.PlayStats.Score   HeadingAlignmentScore  
    number HeadingHiScoreX ScoreboardValuesTopY gameWorld.PlayStats.HiScore HeadingAlignmentHiScore
    number HeadingLevelX   ScoreboardValuesTopY gameWorld.PlayStats.Level   HeadingAlignmentLevel  
    number HeadingLivesX   ScoreboardValuesTopY gameWorld.PlayStats.Lives   HeadingAlignmentLives  

