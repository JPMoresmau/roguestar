HS_FLAGS = 	-hidir products/ \
		-odir products/ \
		-isrc/:products \
		-Wall \
		-Werror \
		-fno-warn-type-defaults \
		--make src/Main.hs \
		-o products/roguestar-engine

default : ghc

clean :
	-rm -f products/*.o 2> /dev/null
	-rm -f products/*.hi 2> /dev/null
	-rm -f products/roguestar-engine 2> /dev/null
	${MAKE} -C haddock clean

doc :
	${MAKE} -C haddock

ghc :
	ghc 	${HS_FLAGS}

ghc-release :
	ghc	-O ${HS_FLAGS}

check:
	${MAKE} clean
	${MAKE} ghc-release
	./products/roguestar-engine tests
	${MAKE} clean
	-darcs whatsnew -l

dist:
	darcs dist

.PHONY : default clean doc ghc ghc-release check dist
