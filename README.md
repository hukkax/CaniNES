# CaniNES

CaniNES is a crossplatform NES emulator built on top of a FreePascal port of
the Mesen emulator core, using the SDL2 framework.

- Mesen is Copyright (C) 2014-2019 M. Bibaud, https://github.com/SourMesen/Mesen
- Currently active fork by various contributors: https://github.com/NovaSquirrel/Mesen
- nes_ntsc is Copyright (C) 2006-2007 Shay Green, http://www.slack.net/~ant/
- FreePascal ports by Joel Toivonen (hukka) 2019-2022, http://hukka.ncn.fi

Donate to the original author of Mesen: [![Donate](https://www.mesen.ca/images/donate.png)](https://www.mesen.ca/Donate.php)

## Features

- Accurate NES emulation as per Mesen
- Recent Files, Favourites, Cheat Selector
- Cartridge information screen with screenshots (requires manual configuration)
- Savestates, Rewind, Fast Forward, Run-ahead
- NTSC and CRT simulation filters

## More information

- [Latest changes](https://github.com/hukkax/CaniNES/blob/main/docs/changes.txt)
- [Known bugs and TODO list](https://github.com/hukkax/CaniNES/blob/main/docs/todo.txt)
- [Supported mappers](https://github.com/hukkax/CaniNES/blob/main/docs/mappers.txt)
- [Known nonworking ROMs](https://github.com/hukkax/CaniNES/blob/main/docs/missing.txt)

## Quick help

Press:
- Left Alt or mouse for menu
- Esc - alternative menu
- Backspace - go back to the previous page in alt. menu
- F1 - toggle info display
- F5 - save console state
- F7 - load console state
- 0-9 - select savestate slot
- F9 - file browser
- F11 - toggle full screen
- Tab - fast forward
- Backspace - rewind (if enabled in settings)
- Arrows and Z,X,Shift,Enter for NES controller 1,B,A,Select,Start;
  numpad for controller 2, or a connected joypad
- Key and joypad bindings are customizable via the Input menu

## Screenshots

![Main menu](https://github.com/hukkax/CaniNES/blob/main/docs/images/mainmenu.png)
![Cartridge information](https://github.com/hukkax/CaniNES/blob/main/docs/images/cartinfo.png)
![HEOH Demo with NTSC and CRT filters](https://github.com/hukkax/CaniNES/blob/main/docs/images/filters.png)
![BattleToads with filters disabled](https://github.com/hukkax/CaniNES/blob/main/docs/images/nofilter.png)
