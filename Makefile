HS_FLAGS = -c -hidir products/ -odir products/ -isrc/:products/
SRC_FILES =  src/Species.hs \
	       	src/DB.hs \
		src/Main.hs \
		src/Tests.hs \
		src/Dice.hs \
		src/Creature.hs \
		src/Alignment.hs \
		src/PeriodicTable.hs \
		src/Creature.hs \
		src/CreatureData.hs

all : products/roguestar-engine

clean :
	-rm -f products/*.o
	-rm -f products/*.hi
	-rm -f products/roguestar-engine
	${MAKE} -C haddock clean

doc :
	${MAKE} -C haddock

products/roguestar-engine : ${SRC_FILES}
	ghc 	-package mtl \
		-hidir products/ \
		-odir products/ \
		-isrc/:products/ \
		--make src/Main.hs \
		-o products/roguestar-engine

.PHONY : all clean
