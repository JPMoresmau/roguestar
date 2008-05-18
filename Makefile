not-a-makefile-base-project:
	@echo "This isn't a Makefile-based project!"
	@echo "You probably want to: runhaskell Setup.hs configure/build/install"

all : doc tests

doc :
	pdflatex -output-directory RSAGL rsagl.tex && cp RSAGL/rsagl.pdf ./

tests: rsagl-tests rsagl-modelview

rsagl-tests :
	ghc --version
	-rm -f rsagl-tests.tix
	ghc -fhpc -prof -auto-all -Wall -Werror -fno-warn-type-defaults --make RSAGL/Tests.hs -main-is RSAGL.Tests.main -o rsagl-tests

optimized : rsagl-tests-optimized rsagl-modelview-optimized

rsagl-tests-optimized :
	ghc --version
	ghc -threaded -Wall -fno-warn-type-defaults -O2 --make RSAGL/Tests.hs -main-is RSAGL.Tests.main -o rsagl-tests-optimized

rsagl-modelview:
	ghc --version
	-rm -f rsagl-modelview.tix
	ghc -fhpc -prof -auto-all -lglut -Wall -Werror -fno-warn-type-defaults --make RSAGL/Main.hs -main-is RSAGL.Main.main -o rsagl-modelview

rsagl-modelview-optimized:
	ghc --version
	ghc -threaded -lglut -Wall -fno-warn-type-defaults -fexcess-precision -O2 --make RSAGL/Main.hs -main-is RSAGL.Main.main -o rsagl-modelview-optimized

colors:
	ghc -lglut -O2 --make RSAGL/ProcessColors.hs -main-is RSAGL.ProcessColors.main -o rsagl-process-colors
	./rsagl-process-colors

darcstest :
	${MAKE} clean
	${MAKE} all
	${MAKE} clean

hpc-metrics: rsagl-tests rsagl-modelview
	-rm -f rsagl-tests.tix
	-rm -f rsagl-modelview.tix
	-rm -f rsagl-sum.tix
	./rsagl-tests
	hpc sum --union rsagl-tests.tix > rsagl-sum.tix
	hpc markup rsagl-sum.tix

clean :
	-rm -f RSAGL/*.dvi
	-rm -f RSAGL/*.aux
	-rm -f RSAGL/*.log
	-rm -f RSAGL/*.pdf
	-rm -f RSAGL/*.toc
	-rm -f RSAGL/*.hi
	-rm -f RSAGL/*.o
	-rm -f RSAGL/*.out
	-rm -f rsagl-tests
	-rm -f rsagl-tests-optimized
	-rm -f rsagl-modelview
	-rm -f rsagl-modelview-optimized
	-rm -f rsagl-process-colors
	-rm -f rsagl.pdf
	-rm -f ./rsagl-modelview.aux
	-rm -f ./rsagl-modelview.hp
	-rm -f ./rsagl-modelview.pdf
	-rm -f ./rsagl-modelview.ps
	-rm -f ./*.tix
	-rm -f hpc_index_alt.html  hpc_index_exp.html  hpc_index_fun.html  hpc_index.html
	-rm -f ./.hpc/*
	-rmdir ./.hpc
	-rm -f ./*.hs.html
	-rm -f ./*.lhs.html

.PHONY : clean doc tests all rsagl-modelview rsagl-modelview-optimized rsagl-tests rsagl-tests-optimized colors hpc-metrics
