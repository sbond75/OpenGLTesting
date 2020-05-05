ghcRoot=$(HOME)/.ghcup/ghc/8.6.5/lib/ghc-8.6.5/
#ghcRoot=/usr/local/Cellar/ghc/8.8.3/lib/ghc-8.8.3/

GHC865=-L"$(ghcRoot)/base-4.12.0.0" -L"$(ghcRoot)/ghc-prim-0.5.3" -L"$(ghcRoot)/integer-gmp-1.0.2.0" -lHSghc-prim-0.5.3 -lHSinteger-gmp-1.0.2.0 -lHSbase-4.12.0.0
GHC883=-L"$(ghcRoot)/base-4.13.0.0" -L"$(ghcRoot)/ghc-prim-0.5.3" -L"$(ghcRoot)/integer-gmp-1.0.2.0" -lHSghc-prim-0.5.3 -lHSinteger-gmp-1.0.2.0 -lHSbase-4.13.0.0

GHCFLAGS=$(GHC865)
#GHCFLAGS=$(GHC883)

all: main.c Picture.o; gcc \
  -liconv \
  -lHSrts \
  -L"$(ghcRoot)/rts" \
  -I"$(ghcRoot)/include" \
  -fno-stack-protector \
  $(GHCFLAGS) \
  -lSDL2 -lgmp -lffi \
  -Wall \
  -o main main.c Picture.o

Picture.o: Picture.hs; ghc -fforce-recomp Picture.hs

.PHONY: clean
clean: ; rm -rf main && rm -rf *.o && rm -rf *.hi && rm -rf *_stub.h
