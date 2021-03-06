/// A cover for some things in the the SDL2CS, to give stronger typing from the F# viewpoint.
module SDLCover

open SDL2
open Fonts


/// SDL2 library initialisation handling.
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


/// SDL Window handle.
[<Struct>]
type WindowNativeInt =
    {
        WindowNativeInt: nativeint
    }

/// SDL BMP image handle.
[<Struct>]
type BMPNativeInt =
    {
        BMPNativeInt: nativeint
    }

/// SDL Surface handle.
[<Struct>]
type SurfaceNativeInt =
    {
        SurfaceNativeInt: nativeint
    }

/// SDL Texture handle.
[<Struct>]
type TextureNativeInt =
    {
        TextureNativeInt: nativeint
    }

/// SDL Renderer handle.
[<Struct>]
type RendererNativeInt =
    {
        RendererNativeInt: nativeint
    }

/// Red, Green, Blue triple.
type RGB =
    {
        Red:   byte
        Green: byte
        Blue:  byte
    }

/// Extended BMP type supports optional transparency colour.
type ExtendedBMP =
    {
        ExBMPNativeInt:      BMPNativeInt
        TransparencyColour:  RGB option
    }



let CreateWindowAndRenderer windowTitle width height =   // TODO: Should we drop back to WithNewMainWindowDo, and separate the creation of the renderer out?

    let windowFlags =
        SDL.SDL_WindowFlags.SDL_WINDOW_SHOWN + 
        SDL.SDL_WindowFlags.SDL_WINDOW_RESIZABLE

    let windowNativeInt =
        SDL.SDL_CreateWindow(windowTitle, 100, 32, width, height, windowFlags)  // TODO: Calculate central position on the screen?

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



/// Convenient constructor for an SDL_Rect.
let ToSdlRect x y w h =
    let mutable r = SDL.SDL_Rect()
    r.x <- x
    r.y <- y
    r.w <- w
    r.h <- h
    r



/// Load a BMP file.  Only returns a value if load was successful.
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

let BMPImagePreparedForRenderer (renderer:RendererNativeInt) (exbmp:ExtendedBMP) =

    let { RendererNativeInt = rendererNativeInt } = renderer
    let { ExBMPNativeInt = { BMPNativeInt = bmpNativeInt } ; TransparencyColour = transp } = exbmp

    let t = typeof<SDL.SDL_Surface>
    let s = (System.Runtime.InteropServices.Marshal.PtrToStructure(bmpNativeInt, t)) :?> SDL.SDL_Surface

    match transp with
        | Some(rgb) ->
            SDL.SDL_SetColorKey(
                bmpNativeInt, 0x00001000 (*SDL_SRCCOLORKEY*),  // TODO: Is this constant seriously not defined in SDL2CS?
                SDL.SDL_MapRGB(s.format, rgb.Red, rgb.Green, rgb.Blue)) |> ignore
        | None ->
            ()

    let texture = SDL.SDL_CreateTextureFromSurface(rendererNativeInt,bmpNativeInt)
    if texture <> 0n then
        Some({
            ImageHandle   = { BMPNativeInt=bmpNativeInt }
            TextureHandle = { TextureNativeInt=texture }
            SourceRect    = ToSdlRect 0 0 s.w s.h
        })
    else
        None


/// Load BMP file and prepare it for the renderer as a texture:
let LoadBMPAndPrepareForRenderer renderer fullPath transparencyColour =
    match LoadBMP fullPath with
        | None -> None
        | Some(bmpNativeInt) -> 
            let exbmp =
                {
                    ExBMPNativeInt = bmpNativeInt
                    TransparencyColour = transparencyColour
                }
            BMPImagePreparedForRenderer renderer exbmp


/// A "NumCaps" font consists of digits 0..9 followed by capital letter A..Z,
/// stored as bitmap image.
type NumCapsFontDefinition =
    {
        FontImage:  BMPSourceImage
        CharWidth:  int
        CharHeight: int
    }

/// Make a "Num Caps" font from a previously-loaded BMPSourceImage.
let MakeNumCapsFontFromBMP (bmpSourceImage:BMPSourceImage) =

    let numGlyphs = 36

    let { ImageHandle=_ ; TextureHandle=_ ; SourceRect=r } = bmpSourceImage

    if r.w % numGlyphs = 0 then
        Some(
            {
                FontImage  = bmpSourceImage
                CharWidth  = r.w / numGlyphs
                CharHeight = r.h
            })
    else
        None



/// Draw a BMPSourceImage onto a surface at a given position.
let DrawImage renderer image left top =
    let {RendererNativeInt=renderer} = renderer
    let mutable dstRect = ToSdlRect left top image.SourceRect.w image.SourceRect.h
    let mutable srcRect = image.SourceRect
    SDL.SDL_RenderCopy(renderer, image.TextureHandle.TextureNativeInt, &srcRect, &dstRect) |> ignore

/// Draw part of a BMPImage onto a surface at a given position.
let DrawSubImage renderer texture srcleft srctop srcwidth srcheight dstleft dsttop dstwidth dstheight =
    let {RendererNativeInt=renderer} = renderer
    let mutable dstRect = ToSdlRect dstleft dsttop dstwidth dstheight
    let mutable srcRect = ToSdlRect srcleft srctop srcwidth srcheight
    SDL.SDL_RenderCopy(renderer, texture.TextureNativeInt, &srcRect, &dstRect) |> ignore

/// Draw a filled rectangle onto the surface at given position in given colour
let DrawFilledRectangle renderer left top right bottom (colourRGB:uint32) =
    let {RendererNativeInt=renderer} = renderer
    let mutable rect = ToSdlRect left top (right-left) (bottom-top)
    SDL.SDL_SetRenderDrawColor(
        renderer, 
        uint8 (colourRGB >>> 16),
        uint8 (colourRGB >>> 8),
        uint8 colourRGB,
        0xFFuy) |> ignore
    SDL.SDL_RenderFillRect(renderer, &rect) |> ignore

/// Draw text at given position in given font, with given alignment.
let DrawTextString renderer x y message textHAlign textVAlign (fontDefinition:NumCapsFontDefinition) =

    let chWidth  = fontDefinition.CharWidth
    let chHeight = fontDefinition.CharHeight
    let texture  = fontDefinition.FontImage.TextureHandle

    let drawCharImage charIndex x y =
        DrawSubImage 
            renderer texture  
            (charIndex * chWidth) 0 chWidth chHeight 
            x y chWidth chHeight
        
    LayOutMonospaceFontTextString drawCharImage chWidth chHeight x y message textHAlign textVAlign


let SetRenderTargetToScreen renderer =
    let { RendererNativeInt=renderer } = renderer
    SDL.SDL_SetRenderTarget(renderer, 0n) |> ignore


let SetRenderTargetToTexture renderer texture =
    let { RendererNativeInt=renderer } = renderer
    let { TextureNativeInt=texture } = texture
    SDL.SDL_SetRenderTarget(renderer, texture) |> ignore


let RenderCopyToFullTarget renderer texture =
    let { RendererNativeInt=renderer } = renderer
    let { TextureNativeInt=texture } = texture
    SDL.SDL_RenderCopy(renderer, texture, 0n, 0n) |> ignore


let Present renderer =
    let { RendererNativeInt=renderer } = renderer
    SDL.SDL_RenderPresent(renderer)


/// Create an RGB 8888 texture for the given renderer.
/// This implies requiring SDL_TEXTUREACCESS_TARGET.
let CreateRgb8888TextureForRenderer renderer textureWidth textureHeight =
    let backingTexture = 
        { 
            TextureNativeInt = 
                SDL.SDL_CreateTexture(
                    renderer.RendererNativeInt, 
                    SDL.SDL_PIXELFORMAT_RGBA8888, 
                    int SDL.SDL_TextureAccess.SDL_TEXTUREACCESS_TARGET, 
                    textureWidth, 
                    textureHeight) 
        }
    if backingTexture.TextureNativeInt = 0n then
        None
    else
        Some(backingTexture)
    