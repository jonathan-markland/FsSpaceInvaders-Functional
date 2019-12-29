module Mechanics

/// Timer ticks aka frame counts.
type TickCount = TickCount of uint32

type TickSpan = TickSpan of uint32

let (---) (TickCount(endTime)) (TickCount(startTime)) = 
    TickSpan(endTime - startTime)

/// Evaluate whether the game elapsed time is an exact multiple
/// of the frequency given, and return (f state) if so.  If not,
/// then just return state without calling f.
let Every (frequency:TickSpan) (elapsedTime:TickSpan) state f =
    let (TickSpan(freq)) = frequency
    let (TickSpan(elapsed)) = elapsedTime
    if (elapsed % freq) = 0u then f state else state
