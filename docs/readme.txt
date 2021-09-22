=============================================================================
CaniNES
=============================================================================

CaniNES is a crossplatform NES emulator built on top of a FreePascal port of
the Mesen emulator core, using the SDL2 framework.

Mesen is Copyright (C) 2014-2019 M. Bibaud, https://github.com/SourMesen/Mesen/
nes_ntsc is Copyright (C) 2006-2007 Shay Green, http://www.slack.net/~ant/
FreePascal ports by Joel Toivonen (hukka) 2019-2021, http://hukka.ncn.fi

=============================================================================
Information
=============================================================================

Why does this exist?

- Learning purposes and wanting a toy project to play around with.
  Initially I tried making my own emulator but I ended up referring to the
  source code of Mesen and various other emulators more and more as time
  passed, and still had problems with PPU and APU emulation, so I decided
  to just port the whole Mesen core instead.

Supported platforms?

- Builds and runs on Windows 10 and Linux. Other platforms untested.

How does this compare to the original Mesen?

- SDL2 used for rendering, audio and input on all platforms.
- No native GUI, just a simple builtin menu system.
- See TODO for list of missing features.

About the code:

- Roughly split in parts: Core, Shell (UI), Basement and graphics library.
- Requires a recent-ish FreePascal version. Can be made to build in Delphi,
  but requires some changes.
- Lots of room for improvement, I'm not a very disciplined programmer.

Core:
- The emulation core is largely a straight translation of the original Mesen
  source, but I have tried to make some optimizations and take advantage
  of Object Pascal's features where it seemed obvious.
  Also includes a port of Blargg's nes_ntsc.
  Could be optimized further, but currently takes around 4-6 ms per frame on
  my old i7 with the NTSC filter enabled, which I deem good enough.

Shell:
- Mainly the menu system, SDL2 interfacing and various utility functions and
  other stuff I thought shouldn't be a part of the emulation core. Should be
  further separated, since some of the core currently makes use of the shell
  stuff.

Basement:
- A simple SDL2-based library I previously wrote to give a simple window with
  a framebuffer you could write to, with pixel scaling, frame timing, input
  handling and audio output plus some utility functions for logging and maths.
  Uses a simple Renderer system to add visual filters and more framebuffers.
  Also contains playroutines for playing back Amiga modules and C-64 SIDs.
  NSF support TBD.

  Renderers:
  - a CRT filter (probably the only thing CaniNES has that Mesen doesn't);
    simulates CRT-type visual stuff including scanlines, dot crawl, blurring
    and scanline bloom using GPU-accelerated blits at the cost of accuracy.
    On the upside it's basically free from a CPU perspective.
  - overlay renderer for overlaying OSD messages and menus on top of the
    main framebuffer.

Graphics library:
- A very cut down version of the Graphics32 library, for use in Basement.

=============================================================================
Quick help
=============================================================================

Press:
 * Esc to show menu
 * Backspace to go back to the previous page in menu
 * F1 to toggle info display
 * F2 to choose game cheats (if available)
 * F3 to toggle NTSC simulation filter
 * F4 to toggle CRT simulation filter (Num -/+ to toggle scanlines)
 * F5 to save console state
 * F7 to load console state
 * F9 for ROM file browser
 * F11 to toggle full screen
 * Tab to fast forward
 * Backspace to rewind (if enabled in settings)
 * 0..9 to select savestate slot
 * I to show catridge info and box art/screenshot (if configured)
- Use arrows and Z,X,Shift,Enter for NES controller 1,B,A,Select,Start;
  numpad for controller 2, or a connected joypad
- Key and joypad bindings are customizable via the Input menu
