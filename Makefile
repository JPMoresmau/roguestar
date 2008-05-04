HS_FLAGS = 	-hidir products \
		-odir products \
		-isrc:products:../rsagl \
		-Wall \
		-fno-warn-type-defaults \
		-fno-warn-unused-imports \
		--make src/Main.lhs \
		-o products/roguestar-gl-bin \
		-fno-warn-unused-imports

default : dev-practical doc

install :
	install products/roguestar-gl /usr/local/bin/
	install products/roguestar-gl-bin /usr/local/bin/

clean :
	-rm -f products/*.o
	-rm -f products/*.hi
	-rm -f products/roguestar-gl
	-rm -f products/roguestar-gl-bin
	-rm -f products/Models/*.o
	-rm -f products/Models/*.hi
	-rm -f products/RSAGL/*.o
	-rm -f products/RSAGL/*.hi
	-rmdir products/Models
	-rmdir products/RSAGL
	${MAKE} -C ../rsagl clean

dev : products/roguestar-gl
	@echo "warning: you're building with development flags on (-Werror, profiling enabled)"
	@echo "         did you want to 'make release' ?"
	ghc	-Werror -prof -auto-all -O1 ${HS_FLAGS}

dev-practical : products/roguestar-gl
	@echo "warning: you're building with development flags on (-Werror)"
	@echo "         did you want to 'make release' ?"
	ghc	-Werror -threaded -O2 ${HS_FLAGS}

release : products/roguestar-gl
	ghc	-O2 -threaded ${HS_FLAGS}

products/roguestar-gl : src/roguestar-gl
	cp src/roguestar-gl products/roguestar-gl
	chmod u+x products/roguestar-gl

.PHONY : default clean dev dev-practical release
