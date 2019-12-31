module Screens

open GamePlayTypes
open InputEventData
open Mechanics
open GamePlay



let CalculateNextScreenState (currentState:Screen) (input:InputEventData) (timeNow:TickCount) =

    match currentState with
        
        | WelcomeScreen(lastHiScore) ->
            if input.FireJustPressed then
                GamePlayScreen (NewGameWorld lastHiScore timeNow)
            else
                currentState

        | GamePlayScreen(world) ->
            match CalculateNextFrameState world input timeNow with
                | GameContinuing(newWorld) -> GamePlayScreen(newWorld)
                | PlayerWon(newWorld)      -> NextLevelResidualAnimationScreen(newWorld,timeNow)
                | PlayerLost(newWorld)     -> 
                    if world.LevelAndLives.Lives > 0u then
                        LifeOverResidualAnimationScreen(newWorld,timeNow)
                    else
                        GameOverResidualAnimationScreen(newWorld,timeNow)

        | NextLevelResidualAnimationScreen(world,screenEntryTime) ->
            match CalculateNextPlaySuspendedState world timeNow screenEntryTime with
                | AnimationContinuing(newWorld) -> NextLevelResidualAnimationScreen(newWorld,screenEntryTime)
                | AnimationFinished(newWorld) -> NextLevelCardScreen(newWorld)

        | NextLevelCardScreen(world) ->
            if input.FireJustPressed then
                GamePlayScreen(world |> NextLevelGameWorld timeNow)
            else
                currentState

        | LifeOverResidualAnimationScreen(world,screenEntryTime) ->
            match CalculateNextPlaySuspendedState world timeNow screenEntryTime with
                | AnimationContinuing(newWorld) -> LifeOverResidualAnimationScreen(newWorld,screenEntryTime)
                | AnimationFinished(newWorld) -> LifeOverCardScreen(newWorld)

        | LifeOverCardScreen(world) ->
            if input.FireJustPressed then
                GamePlayScreen(world |> NextLifeGameWorld timeNow)
            else
                currentState

        | GameOverResidualAnimationScreen(world,screenEntryTime) ->
            match CalculateNextPlaySuspendedState world timeNow screenEntryTime with
                | AnimationContinuing(newWorld) -> GameOverResidualAnimationScreen(newWorld,screenEntryTime)
                | AnimationFinished(newWorld) -> GameOverCardScreen(newWorld)

        | GameOverCardScreen(world) ->
            if input.FireJustPressed then
                WelcomeScreen(world.ScoreAndHiScore.HiScore)
            else
                currentState



let CompletelyNewGameStateWithResetHiScore () =
    let initialHiScore = 500u
    WelcomeScreen(initialHiScore)
