module Fonts

type TextHAlignment = LeftAlign | CentreAlign | RightAlign
type TextVAlignment = TopAlign  | MiddleAlign | BottomAlign

/// Calculate the layout of an aligned monospace text string.
/// Calls 'drawCharImage charIndex LeftX TopY' for each character.
/// The chWidth/chHeight are the dimensions of a character at the target.
/// The font definition must consist of digits 0-9 then capitals A-Z.
let LayOutMonospaceFontTextString drawCharImage chWidth chHeight x y message textHAlign textVAlign  =

    let measuredWidth (s:string) =
        s.Length * chWidth

    let mutable posx =
        match textHAlign with
            | LeftAlign   -> x
            | CentreAlign -> x - (message |> measuredWidth) / 2
            | RightAlign  -> x - (message |> measuredWidth)

    let posy =
        match textVAlign with
            | TopAlign    -> y
            | MiddleAlign -> y - (chHeight / 2)
            | BottomAlign -> y - chHeight

    message |> Seq.iter (fun ch -> 
        let write charIndex = drawCharImage charIndex posx posy
        if      ch >= '0' && ch <= '9' then write ((int ch) - 48)
        else if ch >= 'A' && ch <= 'Z' then write ((int ch) - 55)
        else if ch >= 'a' && ch <= 'z' then write ((int ch) - 87)
        else ()
        posx <- posx + chWidth
    )
