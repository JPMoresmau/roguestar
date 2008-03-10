HS_FLAGS = 	-hidir products \
		-odir products \
		-isrc:products:../rsagl \
		-Wall \
		-fno-warn-type-defaults \
		-fno-warn-unused-imports \
		--make src/Main.lhs \
		-o products/roguestar-gl-bin \
		-fno-warn-unused-imports

default : dev doc

update :
	darcs pull --all

install :
	install products/roguestar-gl /usr/local/bin/
	install products/roguestar-gl-bin /usr/local/bin/

clean :
	-rm -f products/*.o 2> /dev/null
	-rm -f products/*.hi 2> /dev/null
	-rm -f products/roguestar-gl 2> /dev/null
	-rm -f products/roguestar-gl-bin 2> /dev/null
	-rm -f products/Models/*.o 2> /dev/null
	-rm -f products/Models/*.hi 2> /dev/null
	-rmdir products/Models 2> /dev/null
	${MAKE} -C haddock clean
	${MAKE} -C ../rsagl clean

doc :
	${MAKE} -C haddock

dev : products/roguestar-gl
	@echo "warning: you're building with development flags on (-Werror, no optimization)"
	@echo "         did you want to 'make release' ?"
	ghc	-Werror -prof -auto-all ${HS_FLAGS}

release : products/roguestar-gl
	ghc	-O2 -threaded ${HS_FLAGS}

products/roguestar-gl : src/roguestar-gl
	cp src/roguestar-gl products/roguestar-gl
	chmod u+x products/roguestar-gl

.PHONY : default clean doc dev release
