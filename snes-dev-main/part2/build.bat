mkdir out
ca65.exe ./src/main.asm -o ./out/main.o -g
ld65.exe -C ./src/lorom.cfg -o ./out/main.sfc ./out/main.o