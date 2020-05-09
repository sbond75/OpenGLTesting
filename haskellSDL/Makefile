OPTIONS=-O3 -optc-O2 -fasm -fforce-recomp
FILES=main.hs Picture.hs render.c Timing.c

all: $(FILES)
	ghc $(OPTIONS) $(FILES) -lSDL2

run: all
	./main

remake: clean all; ./main
Picture.o: Picture.hs; ghc 

.PHONY: clean
clean: ; rm -rf main && rm -rf *.o && rm -rf *.hi && rm -rf *_stub.h
