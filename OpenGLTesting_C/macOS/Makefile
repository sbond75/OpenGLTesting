
all: main; ./main

main: main.c Picture.o; gcc \
  -liconv \
  -lHSrts \
  -L/usr/local/Cellar/ghc/8.8.3/lib/ghc-8.8.3/rts \
  -I/usr/local/Cellar/ghc/8.8.3/lib/ghc-8.8.3/include \
  -L/usr/local/Cellar/ghc/8.8.3/lib/ghc-8.8.3 \
  -L/usr/local/Cellar/ghc/8.8.3/lib/ghc-8.8.3/base-4.13.0.0 \
  -lHSbase-4.13.0.0 \
  -L/usr/local/Cellar/ghc/8.8.3/lib/ghc-8.8.3/ghc-prim-0.5.3 \
  -lHSghc-prim-0.5.3 \
  -L/usr/local/Cellar/ghc/8.8.3/lib/ghc-8.8.3/integer-gmp-1.0.2.0 \
  -lHSinteger-gmp-1.0.2.0 \
  -lHSghc-prim-0.5.3 \
  -fno-stack-protector \
  -lSDL2 -lgmp -lffi \
  -Wall \
  -o main main.c Picture.o

Picture.o: Picture.hs; ghc -fforce-recomp Picture.hs

.PHONY: clean
clean: ; rm -rf main && rm -rf *.o && rm -rf *.hi && rm -rf *_stub.h
