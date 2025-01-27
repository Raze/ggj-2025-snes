Entry for Global game jam 2025

Rom made in 65c816 Assembly for the Super Nintendo
The game project is located in `bubbles/`

`snes-dev-main/` and `snes-hello-main/` are example projects uses that my code is heavily base on `snes-hello-main/`.
Other resources uses include
- https://ersanio.gitbook.io/assembly-for-the-snes
- https://en.wikibooks.org/wiki/Super_NES_Programming
- https://snes.nesdev.org/wiki/SNESdev_Wiki
- https://youtu.be/57ibhDU2SAI (playlist going over various concepts such as sprites, tiles, pallets)

The ROM is made compiled with cc65 (https://cc65.github.io/)
Use `bubbles/build.bat` or `bubbles/build.sh` to compile (Note: on windows cc65.exe needs to be added to the system variable path in order to use the build.bat as is)
For already compiled ROMs see https://raze-dux.itch.io/gobal-game-jam-jan-2025

The final ROM has mainly been tested in the Mesen 2 emulator (https://github.com/SourMesen/Mesen2), which feature really good debug tools very useful for development.
