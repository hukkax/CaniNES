# CaniNES

CaniNES is a crossplatform NES emulator built on top of a FreePascal port of
the Mesen emulator core, using the SDL2 framework.

- Mesen is Copyright (C) 2014-2019 M. Bibaud, https://github.com/SourMesen/Mesen/
- nes_ntsc is Copyright (C) 2006-2007 Shay Green, http://www.slack.net/~ant/
- FreePascal ports by Joel Toivonen (hukka) 2019-2021, http://hukka.ncn.fi

## Features

- Accurate NES emulation as per Mesen
- Recent Files, Favourites, Cheat Selector
- Cartridge information screen with screenshots (if image locations configured)
- Savestates, Rewind, Fast Forward, Run-ahead
- NTSC and CRT simulation filters

## More information

- [Latest changes](https://github.com/hukkax/CaniNES/blob/main/docs/changes.txt)
- [Known bugs and TODO list](https://github.com/hukkax/CaniNES/blob/main/docs/todo.txt)
- [Supported mappers](https://github.com/hukkax/CaniNES/blob/main/docs/mappers.txt)
- [Known nonworking ROMs](https://github.com/hukkax/CaniNES/blob/main/docs/missing.txt)

## Quick help

Press:
- Esc to show menu
- Backspace to go back to the previous page in menu
- F1 to toggle info display
- F2 to choose game cheats (if available)
- F3 to toggle NTSC simulation filter
- F4 to toggle CRT simulation filter (Num -/+ to toggle scanlines)
- F5 to save console state
- F7 to load console state
- F9 for ROM file browser
- F11 to toggle full screen
- Tab to fast forward
- Backspace to rewind (if enabled in settings)
- 0..9 to select savestate slot
- I to show catridge info and box art/screenshot (if configured)
- Use arrows and Z,X,Shift,Enter for NES controller 1,B,A,Select,Start;
  numpad for controller 2, or a connected joypad
- Key and joypad bindings are customizable via the Input menu

## Screenshots

![Main menu](https://github.com/hukkax/CaniNES/blob/main/docs/images/mainmenu.png)
![Cartridge information](https://github.com/hukkax/CaniNES/blob/main/docs/images/cartinfo.png)
![HEOH Demo with NTSC and CRT filters](https://github.com/hukkax/CaniNES/blob/main/docs/images/filters.png)
![BattleToads with filters disabled](https://github.com/hukkax/CaniNES/blob/main/docs/images/nofilter.png)
