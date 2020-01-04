open SDL2  // TODO: It would be nicer if SDLCover could provide everything.
open System.IO
open SDLCover
open GamePlayTypes
open DrawingCommands
open Rules
open Mechanics
open InputEventData
open Geometry
open ScreenDrawing
open Screens


type SpaceInvadersBMPs =
    {
        Ship:        BMPSourceImage
        RedInvader:  BMPSourceImage
        BlueInvader: BMPSourceImage
        Bullet:      BMPSourceImage
        Bomb:        BMPSourceImage
        Mothership:  BMPSourceImage
        Font:        BMPSourceImage
        Explosion:   BMPSourceImage
    }
  
  

let LoadSpaceInvadersImages (renderer:RendererNativeInt) rootPath =

    let fromFile transparencyColour name = 
        let fullPath = Path.Combine(Path.Combine(rootPath, "Images"), name) + ".bmp"
        match LoadBMPAndPrepareForRenderer renderer fullPath transparencyColour with
            | Some(imageRecord) -> imageRecord
            | None -> failwith (sprintf "Space invaders could not start because file '%s' is missing or has invalid content." fullPath)

    let magenta = Some({ Red=255uy ; Green=0uy ; Blue=255uy })

    {
        Ship        = fromFile magenta "Ship"
        RedInvader  = fromFile magenta "RedInvader"
        BlueInvader = fromFile magenta "BlueInvader"
        Bullet      = fromFile magenta "Bullet"
        Bomb        = fromFile magenta "Bomb"
        Mothership  = fromFile magenta "Mothership"
        Font        = fromFile magenta "Font"
        Explosion   = fromFile magenta "Explosion"
    }



/// Render game drawing command to the screen.
/// Here we are choosing to use a 1:1 mapping from world coordinates onto
/// a 256 x 256 pixel SDL surface.
let RenderToSdl imageSet fontDefinition renderer drawingCommand =

    /// Convert World Units <wu> to our pixels.  It happens that this is 1:1 with 256 x 256 schemes.
    let px (n:int<wu>) = int n

    match drawingCommand with
        
        | DrawBullet(left,top) ->
            DrawImage renderer imageSet.Bullet (px left) (px top)

        | DrawBomb(left,top) ->
            DrawImage renderer imageSet.Bomb (px left) (px top)

        | DrawInvader(left,top,dogTag) ->
            let invaderBmp =
                match (InvaderColourFromDogTag dogTag) with
                    | RedInvader  -> imageSet.RedInvader
                    | BlueInvader -> imageSet.BlueInvader
            DrawImage renderer invaderBmp (px left) (px top)

        | DrawMothership(left,top) ->
            DrawImage renderer imageSet.Mothership (px left) (px top)

        | DrawShip(left,top) ->
            DrawImage renderer imageSet.Ship (px left) (px top)

        | DrawExplosion(e) ->
            let w = imageSet.Explosion.SourceRect.w
            let h = imageSet.Explosion.SourceRect.h
            let dstx = (px e.ExplosionExtents.LeftW) 
            let dsty = (px e.ExplosionExtents.TopW) 
            let dstw = (px e.ExplosionExtents.RightW) - dstx 
            let dsth = (px e.ExplosionExtents.BottomW) - dsty 
            DrawSubImage 
                renderer
                imageSet.Explosion.TextureHandle
                0 0 w h
                dstx dsty dstw dsth

        | DrawText(x,top,message,textHAlign,textVAlign) ->
            DrawTextString renderer (px x) (px top) message textHAlign textVAlign fontDefinition

        | TitleBackground ->
            DrawFilledRectangle renderer 0 0 256 256 0x000040u

        | GameplayBackground ->
            DrawFilledRectangle renderer 0 0 256 256 0u

        | GameOverBackground ->
            DrawFilledRectangle renderer 0 0 256 256 0x400000u
    
        | NextLevelBackground ->
            DrawFilledRectangle renderer 0 0 256 256 0x400040u

        | LifeOverBackground ->
            DrawFilledRectangle renderer 0 0 256 256 0x004000u




let TimerCallback (interval:uint32) (param:nativeint) : uint32 =  // TODO: Can this go into the SDL library?

    let mutable event = new SDL.SDL_Event()

    event.``type`` <- SDL.SDL_EventType.SDL_USEREVENT
    event.user.code <- 0
    event.user.data1 <- 0n
    event.user.data2 <- 0n

    SDL.SDL_PushEvent(&event) |> ignore
    interval  // We can return 0u to cancel the timer here, or interval to keep it going.


(*
let SpikeSdlRendererMain () =

    let imageSet = LoadSpaceInvadersImages ""
    
    let mutable windowNativeInt = 0n
    let mutable rendererNativeInt = 0n
    let createWRResult = SDL.SDL_CreateWindowAndRenderer(320,240,SDL.SDL_WindowFlags.SDL_WINDOW_RESIZABLE,&windowNativeInt,&rendererNativeInt)
    if createWRResult = 0 then

        let mutable renInfo = new SDL.SDL_RendererInfo ()
        SDL.SDL_GetRendererInfo(rendererNativeInt, &renInfo) |> ignore

        // SDL.SDL_RendererFlags.SDL_RENDERER_TARGETTEXTURE
        // SDL.SDL_RendererFlags.SDL_RENDERER_ACCELERATED

        let texture = SDL.SDL_CreateTextureFromSurface(rendererNativeInt,imageSet.BlueInvader.ImageHandle.BMPHandle)
        if texture <> 0n then
            let mutable stopping = false
            while not stopping do
                let mutable event = new SDL.SDL_Event()
                SDL.SDL_PollEvent(&event) |> ignore
                if (event.``type`` = SDL.SDL_EventType.SDL_QUIT) then
                    stopping <- true
                else
                    SDL.SDL_SetRenderDrawColor(rendererNativeInt,0uy,0uy,0uy,0uy) |> ignore
                    SDL.SDL_RenderClear(rendererNativeInt) |> ignore
                    SDL.SDL_RenderCopy(rendererNativeInt, texture, 0n, 0n) |> ignore
                    SDL.SDL_RenderPresent(rendererNativeInt)

    // TODO : This spike does not cleanly relese stuff
    
    0
*)
            



let GameMain () =

    // TODO:  Minor: We don't actually free the imageSet handles.

    match CreateWindowAndRenderer "Space Invaders" 800 800 with   // TODO: constants
        | Some(mainWindow, renderer) ->

            let backingTexture = CreateRgb8888TextureForRenderer renderer 256 256
            match backingTexture with
                | None -> failwith "Cannot create backing texture."
                | Some(backingTexture) ->

                    let imageSet = LoadSpaceInvadersImages renderer ""

                    match MakeNumCapsFontFromBMP imageSet.Font with
                        | None -> 
                            0
                        | Some(fontDefinition) ->
                            let mutable screenState = CompletelyNewGameStateWithResetHiScore ()
 
                            let timerID =   // TODO: Push into library?
                                SDL.SDL_AddTimer(20u,new SDL.SDL_TimerCallback(TimerCallback),0n)
            
                            if timerID = 0 then
                                failwith "Failed to install the gameplay timer."

                            let renderFunction = (RenderToSdl imageSet fontDefinition renderer)

                            let mutable leftHeld = false
                            let mutable rightHeld = false
                            let mutable tickCount = 0u

                            let mutable fireJustPressed = false  // until discovered otherwise
                            let mutable fireWaitingRelease = false

                            let mutable stop = false // TODO: hack

                            let mutable quit = false
                            while quit = false do

                                let mutable event = new SDL.SDL_Event ()

                                while (SDL.SDL_WaitEvent (&event)) <> 0 && not quit do   // SDL_PollEvent

                                    let msg = event.``type``

                                    if msg = SDL.SDL_EventType.SDL_QUIT then 
                                        quit <- true

                                    else if msg = SDL.SDL_EventType.SDL_KEYDOWN then
                                        match event.key.keysym.scancode with
                                            | SDL.SDL_Scancode.SDL_SCANCODE_LEFT  -> leftHeld <- true
                                            | SDL.SDL_Scancode.SDL_SCANCODE_RIGHT -> rightHeld <- true
                                            | SDL.SDL_Scancode.SDL_SCANCODE_Z     -> 
                                                if fireWaitingRelease 
                                                then () 
                                                else 
                                                    fireJustPressed <- true
                                                    fireWaitingRelease <- true
                                            | _ -> ()

                                    else if msg = SDL.SDL_EventType.SDL_KEYUP then
                                        match event.key.keysym.scancode with
                                            | SDL.SDL_Scancode.SDL_SCANCODE_LEFT  -> leftHeld <- false
                                            | SDL.SDL_Scancode.SDL_SCANCODE_RIGHT -> rightHeld <- false
                                            | SDL.SDL_Scancode.SDL_SCANCODE_Z     -> fireWaitingRelease <- false
                                            | _ -> ()

                                    else if msg = SDL.SDL_EventType.SDL_USEREVENT then
                                        // ~ This is the AddTimer event handler 
                                        tickCount <- tickCount + 1u
                                        let inputEventData = { LeftHeld=leftHeld ; RightHeld=rightHeld ; FireJustPressed=fireJustPressed }
                                        let nextState = CalculateNextScreenState screenState inputEventData (TickCount(tickCount))
                                        SetRenderTargetToTexture renderer backingTexture
                                        RenderScreen renderFunction nextState
                                        SetRenderTargetToScreen renderer
                                        RenderCopyToFullTarget renderer backingTexture
                                        Present renderer
                                        fireJustPressed <- false
                                        screenState <- nextState
                            1

        | None ->
            0
            


(*
let RendererPerformanceSpikeMain () =

    match CreateWindowAndRenderer "Performance spike" 1920 1080 with   // TODO: constants
        | Some(mainWindow, renderer) ->

            let backingTexture = { TextureNativeInt = SDL.SDL_CreateTexture(renderer.RendererNativeInt, SDL.SDL_PIXELFORMAT_RGBA8888, int SDL.SDL_TextureAccess.SDL_TEXTUREACCESS_TARGET, 256, 256) }
            if backingTexture.TextureNativeInt = 0n then
                failwith "" // TODO: sort out

            let imageSet = LoadSpaceInvadersImages renderer ""

            match MakeNumCapsFontFromBMP imageSet.Font with
                | None -> 
                    0
                | Some(fontDefinition) ->

                    let total = 1000u
                    let mutable counter = 0u
                    let timeAtStart = System.DateTime.Now

                    while counter < total do
                        // Garbage test:
                        // [1n..5000n] |> List.map (fun item -> item.ToString()) |> ignore
                        SetRenderTargetToTexture renderer backingTexture
                        let c = (0xFF000000u + (counter &&& 0xFFu))
                        DrawFilledRectangle renderer 0 0 256 256 c
                        SetRenderTargetToScreen renderer
                        RenderCopyToFullTarget renderer backingTexture
                        Present renderer
                        counter <- (counter + 1u)
                        
                    let timeAtEnd = System.DateTime.Now
                    let elapsed = timeAtEnd.Subtract(timeAtStart)
                    printfn "Elapsed: %g milliseconds per frame" (elapsed.TotalMilliseconds / float total)

                    1

        | None ->
            0




let RendererWithTimerSpikeMain () =

    match CreateWindowAndRenderer "Renderer and timer spike" 1920 1080 with   // TODO: constants
        | Some(mainWindow, renderer) ->

            let backingTexture = { TextureNativeInt = SDL.SDL_CreateTexture(renderer.RendererNativeInt, SDL.SDL_PIXELFORMAT_RGBA8888, int SDL.SDL_TextureAccess.SDL_TEXTUREACCESS_TARGET, 256, 256) }
            if backingTexture.TextureNativeInt = 0n then
                failwith "" // TODO: sort out

            let timerID = 
                SDL.SDL_AddTimer(100u,new SDL.SDL_TimerCallback(TimerCallback),0n)
            
            if timerID = 0 then
                failwith "Failed to install the gameplay timer."

            let imageSet = LoadSpaceInvadersImages renderer ""

            match MakeNumCapsFontFromBMP imageSet.Font with
                | None -> 
                    0
                | Some(fontDefinition) ->

                    let mutable counter = 0u

                    let mutable quit = false
                    while quit = false do

                        let mutable event = new SDL.SDL_Event ()

                        while (SDL.SDL_WaitEvent (&event)) <> 0 && not quit do   // SDL_PollEvent

                            let msg = event.``type``

                            if msg = SDL.SDL_EventType.SDL_QUIT then 
                                quit <- true

                            else if msg = SDL.SDL_EventType.SDL_USEREVENT then
                                SetRenderTargetToTexture renderer backingTexture
                                let c = (0xFF000000u + (counter &&& 0xFFu))
                                DrawFilledRectangle renderer 0 0 256 256 c
                                SetRenderTargetToScreen renderer
                                RenderCopyToFullTarget renderer backingTexture
                                Present renderer
                                counter <- (counter + 1u)

                    1

        | None ->
            0
*)



[<EntryPoint>]
let main argv =
    match WithSdl2Do GameMain with //GameMain with
        | None -> 
            printfn "Failed to start SDL2 library."   // TODO: Let's not use the STDOUT.
            0
        | Some(n) -> n


 

