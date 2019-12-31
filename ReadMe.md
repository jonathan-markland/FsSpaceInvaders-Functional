FsSpaceInvaders-Functional
==========================

A slightly-shitty space invaders.

I'm not really that interested in how good this is as space invaders!
This is a "game development in F#" spike program.

This follows on from the *FsSpaceInvaders-Imperative* project.

This version does *not* use any mutable state for the primary
game "world" model.

GC Performance measurements
---------------------------
I did a spike-test for frame rate of a solid filled screen, and I 
obtained ~200fps on my laptop.  This was with no garbage.

I then generated 5,000 items of collectable garbage *per frame*, and saw
a significant increase in the number of collections happening, but the 
frame rate was not substantially impacted.

Since I saw ~50fps as plenty, I felt that GC would not be a problem for
retro game development in this manner, not withstanding any optimisations
that could be made by the programmer to reduce garbage using:

	- Using [<Struct>] to keep things off the heap
	- Re-using previous instances if no state changed during the frame,
	  which might be quite important for things like 'List.map'.

Intentions
----------
This is in F#, and is intended to target Windows and Linux via .Net
Core, using the "SDL2" library, and the "SDL2-CS" interop library.

It is also intended to be recompiled with the F# Fable compiler to
target Javascript, which will require the SDL2 back-end stripping
off and replacing with HTML Canvas (I think -- unless advised
otherwise).  I would also need to find out best practice 
recommendations for games in the browser, eg: timers / input etc...

Status
------
Only Windows .NET is developer-tested.
Linux is untried at the time of writing.
Fable/JS back-end code isn't started (no code).

Framework Notes
---------------
This was developed in .Net Framework, but then I manually switched it
to .Net Core by fiddling some files.  These fiddlings may have left
behind configuration artefacts I still need to remove.  Possibly!

Libraries
=========

FsAnyGame
---------
This library is for things that are not particularly specific to Space Invaders.

FsSpaceInvaders
---------------
This library contains the game engine.

This includes the types needed to define the data model for the screens and
the main gameplay (the "GameWorld").  It defines World Units "<wu>" using the F#
units-of-measure feature, and models gameplay in a 256<wu> x 256<wu> space.

This library uses Dependency Inversion Principle so that there is *no dependency* 
on the I/O framework.  To clarify, while the main program uses SDL2, this means 
there are *no* SDL2 types used in this library.

Input is fed in through the InputEventData type.

output is achieved by the "Render" functions, which pass DrawingCommand
values to a client-defined callback.  The client supplies the entire I/O
framework, and graphic images.  The client translates <wu> units to the target
display as it sees fit.

FsSpaceInvadersSDL2CS
---------------------
This is the main executable.

This has direct dependencies on the SDL2 DLL and the C# "SDL2-CS" interop library.

The SDL2-CS interop library must be placed in the parent of this solution's folder,
see the <Reference>:

    <Reference Include="SDL2-CS">
      <HintPath>..\..\SDL2-CS\bin\Debug\SDL2-CS.dll</HintPath>
    </Reference>

This creates a window and renderer using SDL2, and provides the graphics images
for the game.

This chooses to render the game engine's <wu> World Units to a 256 x 256 off-screen
texture bitmap, and uses the GPU to stretch blit the image across the host's window.

Future Work
===========
I am hoping that SDL2-CS works on Linux.  Obviously, the Linux build of the SDL2 DLL
would be needed, but I am hoping the interop will work.

Since I need a small amount of the SDL2-CS interop, I *could* copy-paste that, and
deal with the interop myself, but I think that would only happen if there was a
technical problem with interfacing the SDL DLL on linux.

FsSpaceInvadersSDL2CS has some SDL interfacing routines that are better from an F#
perspective than using the SDL2-CS interop directly.  These are in the SDLCover.fs
file, and this could be brought into a separate DLL for future games that want to
have an SDL2 back end.

Making a second game would be the best catalyst for reviewing the code from a
reusability perspective.  FsSpaceInvaders should have *no* re-usable content by
its very nature.

