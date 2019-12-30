module InputEventData

[<Struct>]
type InputEventData =
    {
        /// The LEFT button is repeating
        LeftHeld:         bool

        /// The RIGHT button is repeating
        RightHeld:        bool

        /// The FIRE button does not repeat
        FireJustPressed:  bool
    }



let InputsWhereNothingIsPressed =
    {
        LeftHeld = false
        RightHeld = false
        FireJustPressed = false
    }