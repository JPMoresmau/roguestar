HS_FLAGS = -hidir products/ -odir products/ -isrc/:products/ --make src/Main.hs -o products/roguestar-engine

default : ghc

clean :
	-rm -f products/*.o
	-rm -f products/*.hi
	-rm -f products/roguestar-engine
	${MAKE} -C haddock clean

doc :
	${MAKE} -C haddock

ghc :
	ghc 	-Wall ${HS_FLAGS}

ghc-release :
	ghc	-O ${HS_FLAGS}

.PHONY : default clean doc ghc ghc-release
