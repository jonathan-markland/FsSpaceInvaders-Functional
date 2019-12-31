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

