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
                | PlayerWon(newWorld)      -> NextLevelAminScreen(newWorld,timeNow)
                | PlayerLost(newWorld)     -> 
                    if world.LevelAndLives.Lives > 0u then
                        LifeOverAnimScreen(newWorld,timeNow)
                    else
                        GameOverAnimScreen(newWorld,timeNow)

        | NextLevelAminScreen(world,endedAt) ->
            match CalculateNextPlaySuspendedState world timeNow endedAt with
                | AnimationContinuing(newWorld) -> NextLevelAminScreen(newWorld,endedAt)
                | AnimationFinished(newWorld) -> NextLevelScreen(newWorld)

        | NextLevelScreen(world) ->
            if input.FireJustPressed then
                GamePlayScreen(world |> NextLevelGameWorld timeNow)
            else
                currentState

        | LifeOverAnimScreen(world,endedAt) ->
            match CalculateNextPlaySuspendedState world timeNow endedAt with
                | AnimationContinuing(newWorld) -> LifeOverAnimScreen(newWorld,endedAt)
                | AnimationFinished(newWorld) -> LifeOverScreen(newWorld)

        | LifeOverScreen(world) ->
            if input.FireJustPressed then
                GamePlayScreen(world |> NextLifeGameWorld timeNow)
            else
                currentState

        | GameOverAnimScreen(world,endedAt) ->
            match CalculateNextPlaySuspendedState world timeNow endedAt with
                | AnimationContinuing(newWorld) -> GameOverAnimScreen(newWorld,endedAt)
                | AnimationFinished(newWorld) -> GameOverScreen(newWorld)

        | GameOverScreen(world) ->
            if input.FireJustPressed then
                WelcomeScreen(world.ScoreAndHiScore.HiScore)
            else
                currentState



let CompletelyNewGameStateWithResetHiScore () =
    let initialHiScore = 500u
    WelcomeScreen(initialHiScore)
