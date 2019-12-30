/// A cover for some things in the the SDL2CS, to give stronger typing from the F# viewpoint.
module SDLCover

// TODO:  https://stackoverflow.com/questions/33402909/sdl-renderpresent-vs-sdl-updatewindowsurface

(*
If you store your images in RAM and use the CPU for rendering(this is called software rendering) you use SDL_UpdateWindowSurface.
By calling this function you tell the CPU to update the screen and draw using software rendering.
You can store your textures in RAM by using SDL_Surface, but software rendering is inefficent. You can give draw calls by using SDL_BlitSurface.
SDL_UpdateWindowSurface is equivalent to the SDL 1.2 API SDL_Flip().
On the other side when you use the GPU to render textures and you store your texture on the GPU(this is called hardware accelerated rendering), which you should, you use SDL_RenderPresent.
This function tells the GPU to render to the screen.
You store texture on the GPU using SDL_Texture. When using this you can give draw calls by using SDL_RenderCopy or if you want transformations SDL_RenderCopyEx
Therefore, when using SDL's rendering API, one does all drawing intended for the frame, and then calls this function once per frame to present the final drawing to the user.
You should you use hardware rendering it's far more efficent, than software rendering! Even if the user running the program hasn't got a GPU (which is rare, because most CPU's have an integrated GPU) SDL will switch to software rendering by it self!
By the way you can load an image as SDL_Texture without the need to load an image as an SDL_Surface and convert it to an SDL_Texture using the SDL_image library, which you should because it supports several image formats not just BMP, like pure SDL. (SDL_image is made by the creators of SDL)
Just use the IMG_LoadTexture from SDL_image!

share
improve this answer
edited Oct 29 '15 at 11:07 


answered Oct 29 '15 at 10:37 

kovacsmarcell 
2111
1 silver badge
8
8 bronze badges
 
 *)

open SDL2
open Fonts

[<Struct>]
type WindowNativeInt =
    {
        WindowNativeInt: nativeint
    }

[<Struct>]
type BMPNativeInt =
    {
        BMPNativeInt: nativeint
    }

[<Struct>]
type SurfaceNativeInt =
    {
        SurfaceNativeInt: nativeint
    }

[<Struct>]
type TextureNativeInt =
    {
        TextureNativeInt: nativeint
    }

[<Struct>]
type RendererNativeInt =
    {
        RendererNativeInt: nativeint
    }



let CreateWindowAndRenderer width height =   // TODO: Should we drop back to WithNewMainWindowDo, and separate the creation of the renderer out?

    let windowFlags =
        SDL.SDL_WindowFlags.SDL_WINDOW_SHOWN + 
        SDL.SDL_WindowFlags.SDL_WINDOW_RESIZABLE

    let windowNativeInt =
        SDL.SDL_CreateWindow("GAME", 100, 32, width, height, windowFlags)  // TODO: Calculate central position on the screen?

    if windowNativeInt <> 0n then

        let renderFlags =
            SDL.SDL_RendererFlags.SDL_RENDERER_ACCELERATED

        let rendererNativeInt =
            SDL.SDL_CreateRenderer(windowNativeInt, -1, renderFlags)

        if rendererNativeInt <> 0n then
            Some({ WindowNativeInt=windowNativeInt } , { RendererNativeInt=rendererNativeInt })
        else
            SDL.SDL_DestroyWindow(windowNativeInt)
            None

    else
        None




let ToSdlRect x y w h =

    let mutable r = SDL.SDL_Rect()
    r.x <- x
    r.y <- y
    r.w <- w
    r.h <- h
    r



let LoadBMP filePath =
    let handle = SDL.SDL_LoadBMP(filePath)
    if handle = nativeint 0 then
        None
    else
        Some({ BMPNativeInt = handle })



type BMPSourceImage =
    {
        ImageHandle:   BMPNativeInt
        TextureHandle: TextureNativeInt
        SourceRect:    SDL.SDL_Rect
    }

let BMPImagePreparedForRenderer rendererNativeInt { BMPNativeInt = bmp } =

    let t = typeof<SDL.SDL_Surface>
    let s = (System.Runtime.InteropServices.Marshal.PtrToStructure(bmp, t)) :?> SDL.SDL_Surface

    let texture = SDL.SDL_CreateTextureFromSurface(rendererNativeInt,bmp)
    if texture <> 0n then
        Some({
            ImageHandle   = { BMPNativeInt=bmp }
            TextureHandle = { TextureNativeInt=texture }
            SourceRect    = ToSdlRect 0 0 s.w s.h
        })
    else
        None



type FontDefinition =
    {
        FontImageHandle:  BMPNativeInt
        FontTextureNativeInt: TextureNativeInt
        CharWidth:        int
        CharHeight:       int
    }


let MakeFontFromBMP { RendererNativeInt=renderer } { BMPNativeInt=bmp } =

    let texture = SDL.SDL_CreateTextureFromSurface(renderer,bmp)
    if texture <> 0n then
        Some({
            FontImageHandle      = { BMPNativeInt=bmp }
            FontTextureNativeInt = { TextureNativeInt=texture }
            CharWidth  = 6  // TODO
            CharHeight = 8  // TODO
        })
    else
        None







let UpdateWindowSurface {WindowNativeInt=h} =
    SDL.SDL_UpdateWindowSurface h |> ignore

let WithNewMainWindowDo windowTitleString windowWidth windowHeight operation =
    
    let window = 
        SDL.SDL_CreateWindow(
            windowTitleString, 
            100, 100, 
            windowWidth, windowHeight, 
            SDL.SDL_WindowFlags.SDL_WINDOW_SHOWN)

    if window = nativeint 0 then
        Error (sprintf "Window could not be created! SDL_Error: %s\n" (SDL.SDL_GetError ()))
    else
        try
            let operationResult = operation {WindowNativeInt = window}
            SDL.SDL_DestroyWindow(window)
            Ok (operationResult)
        with e ->
            Error (e.Message)



let WithWindowSurfaceDo operation {WindowNativeInt=wh} =

    let windowSurface = SDL.SDL_GetWindowSurface(wh)

    if windowSurface = 0n then
        Error (sprintf "Window surface could not be obtained! SDL_Error: %s\n" (SDL.SDL_GetError ()))
    else
        Ok (operation {SurfaceNativeInt = windowSurface})



/// Draw a BMPSourceImage onto a surface at a given position.
let DrawImage {RendererNativeInt=renderer} (image:BMPSourceImage) left top =
    let mutable dstRect = ToSdlRect left top image.SourceRect.w image.SourceRect.h
    let mutable srcRect = image.SourceRect
    // SDL.SDL_BlitSurface (image.ImageHandle.BMPNativeInt, &srcRect, screenSurface, &dstRect) |> ignore
    SDL.SDL_RenderCopy(renderer, image.TextureHandle.TextureNativeInt, &srcRect, &dstRect) |> ignore

/// Draw part of a BMPImage onto a surface at a given position.
let DrawSubImage {RendererNativeInt=renderer} (texture:TextureNativeInt) srcleft srctop srcwidth srcheight dstleft dsttop dstwidth dstheight =
    let mutable dstRect = ToSdlRect dstleft dsttop dstwidth dstheight
    let mutable srcRect = ToSdlRect srcleft srctop srcwidth srcheight
    // SDL.SDL_BlitSurface (imageHandle.BMPNativeInt, &srcRect, screenSurface, &dstRect) |> ignore
    SDL.SDL_RenderCopy(renderer, texture.TextureNativeInt, &srcRect, &dstRect) |> ignore

/// Draw a filled rectangle onto the surface at given position in given colour
let DrawFilledRectangle {RendererNativeInt=renderer} left top right bottom (colourRGB:uint32) =
    let mutable rect = ToSdlRect left top (right-left) (bottom-top)
    // SDL.SDL_FillRect (screenSurface, &rect, fillColour) |> ignore
    SDL.SDL_SetRenderDrawColor(
        renderer, 
        uint8 (colourRGB >>> 16),
        uint8 (colourRGB >>> 8),
        uint8 colourRGB,
        0xFFuy) |> ignore
    SDL.SDL_RenderFillRect(renderer, &rect) |> ignore

/// Draw text at given position in given font, with given alignment.
let DrawTextString renderer x y message textHAlign textVAlign (fontDefinition:FontDefinition) =

    let chWidth  = fontDefinition.CharWidth
    let chHeight = fontDefinition.CharHeight
    let texture  = fontDefinition.FontTextureNativeInt

    let drawCharImage charIndex x y =
        DrawSubImage 
            renderer texture  
            (charIndex * chWidth) 0 chWidth chHeight 
            x y chWidth chHeight
        
    LayOutMonospaceFontTextString drawCharImage chWidth chHeight x y message textHAlign textVAlign



let WithSdl2Do f =
    try
        let initResult = SDL.SDL_Init(SDL.SDL_INIT_TIMER)
        if initResult = 0 then
            Some(f ())
        else
            None
    with 
        | :? System.BadImageFormatException ->
            None


let SetRenderTargetToScreen { RendererNativeInt=renderer } =
    SDL.SDL_SetRenderTarget(renderer, 0n) |> ignore


let SetRenderTargetToTexture { RendererNativeInt=renderer } { TextureNativeInt=texture } =
    SDL.SDL_SetRenderTarget(renderer, texture) |> ignore


let RenderCopyToFullTarget { RendererNativeInt=renderer } { TextureNativeInt=texture } =
    SDL.SDL_RenderCopy(renderer, texture, 0n, 0n) |> ignore


let Present {RendererNativeInt=renderer} =
    SDL.SDL_RenderPresent(renderer)


    