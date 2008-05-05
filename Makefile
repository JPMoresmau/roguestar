HS_FLAGS = 	-hidir products/ \
		-odir products/ \
		-isrc/:products \
		-Wall \
		-fno-warn-type-defaults \
		--make src/Main.hs \
		-o products/roguestar-engine

default : roguestar-engine

update :
	darcs pull --all

install :
	install products/roguestar-engine /usr/local/bin/

clean :
	-rm -f products/*.o 2> /dev/null
	-rm -f products/*.hi 2> /dev/null
	-rm -f products/roguestar-engine 2> /dev/null

roguestar-engine :
	@echo "warning: you're building with development flags on (-Werror, no optimization)"
	@echo "         did you want to 'make release' ?"
	ghc-6.8.2 	-Werror ${HS_FLAGS} -prof -auto-all

release :
	ghc	-O2 ${HS_FLAGS}

dist:
	darcs dist

.PHONY : default clean ghc ghc-release dist headache headache-remove release
