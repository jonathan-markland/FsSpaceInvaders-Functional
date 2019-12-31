module ScreenDrawing

open Dimensions
open Fonts
open DrawingCommands
open GameDrawing
open GamePlayTypes



let private text x top message alignmentH alignmentV =
    DrawText (x, top, message, alignmentH, alignmentV)    // TODO: Text not vertically centred in the screen.



let RenderWelcomeScreen render =
    render (TitleBackground)
    render (text (ScreenWidth / 2) (ScreenHeight / 2)       "SPACE INVADERS"     CentreAlign MiddleAlign)
    render (text (ScreenWidth / 2) ((ScreenHeight * 6) / 7) "PRESS FIRE TO PLAY" CentreAlign MiddleAlign)



let RenderGamePlayScreen render gameWorld =  // TODO: bool param!
    RenderGameWorld render gameWorld



let RenderNextLevelScreen render =
    render (NextLevelBackground)
    render (text (ScreenWidth / 2) (ScreenHeight / 2) "NEXT LEVEL   WELL DONE" CentreAlign MiddleAlign)
    


let RenderLifeOverScreen render =
    render (LifeOverBackground)
    render (text (ScreenWidth / 2) (ScreenHeight / 2) "SHIP DESTROYED" CentreAlign MiddleAlign)



let RenderGameOverScreen render =
    render (GameOverBackground)
    render (text (ScreenWidth / 2) (ScreenHeight / 2) "GAME OVER" CentreAlign MiddleAlign)



let RenderScreen render screen =
    match screen with
        | WelcomeScreen(_)             -> RenderWelcomeScreen render
        | GamePlayScreen(world)
        | GameOverResidualAnimationScreen(world,_)
        | LifeOverResidualAnimationScreen(world,_)
        | NextLevelResidualAnimationScreen(world,_) -> RenderGamePlayScreen  render world
        | NextLevelCardScreen(_)           -> RenderNextLevelScreen render
        | LifeOverCardScreen(_)            -> RenderLifeOverScreen  render
        | GameOverCardScreen(_)            -> RenderGameOverScreen  render


