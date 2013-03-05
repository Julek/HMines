all:
	ghc -O3 -fllvm --make HMines.hs
	rm *.hi *.o
