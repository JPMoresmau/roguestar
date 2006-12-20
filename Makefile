HS_FLAGS = 	-hidir products/ \
		-odir products/ \
		-isrc/:products \
		-Wall \
		-fno-warn-type-defaults \
		-fno-warn-unused-imports \
		-lglut \
		--make src/Main.hs \
		-o products/roguestar-gl-bin

# -fno-warn-unused-imports due to an apparent bug in ghc, remove this and try again later

default : ghc doc

update :
	darcs pull

release : ghc-release

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

doc :
	${MAKE} -C haddock

ghc-prof : products/roguestar-gl
	ghc 	-prof -auto-all ${HS_FLAGS}

ghc : products/roguestar-gl
	@echo "warning: you're building with development flags on (-Werror, no optimization)"
	@echo "         did you want to 'make release' ?"
	ghc	-Werror ${HS_FLAGS}

ghc-release : products/roguestar-gl
	ghc	-O ${HS_FLAGS}

products/roguestar-gl : src/roguestar-gl
	cp src/roguestar-gl products/roguestar-gl
	chmod u+x products/roguestar-gl

check:
	${MAKE} clean
	${MAKE} ghc-release
	${MAKE} clean
	-darcs whatsnew -ls

dist:
	darcs dist

headache:
	headache -c header/license-header.conf -h header/license-header src/*.hs

headache-remove:
	headache -c header/license-header.conf -h header/license-header -r src/*.hs

.PHONY : default clean doc ghc ghc-release check dist headache headache-remove release
