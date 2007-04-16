all : doc tests

doc :
	(cd RSAGL && latex rsagl.tex && latex rsagl.tex && latex rsagl.tex && dvipdf rsagl.dvi)
	cp RSAGL/rsagl.pdf ./rsagl.pdf

tests :
	ghc -Wall -fno-warn-type-defaults --make RSAGL/Tests.hs -main-is RSAGL.Tests.main -o rsagl-tests

modelview:
	ghc -lglut -Wall -fno-warn-type-defaults --make RSAGL/Main.hs -main-is RSAGL.Main.main -o rsagl-modelview

clean :
	-rm -f RSAGL/*.dvi
	-rm -f RSAGL/*.aux
	-rm -f RSAGL/*.log
	-rm -f RSAGL/*.pdf
	-rm -f RSAGL/*.toc
	-rm -f RSAGL/*.hi
	-rm -f RSAGL/*.o
	-rm -f rsagl-tests
	-rm -f ./rsagl-modelview
	-rm -f rsagl.pdf

.PHONY : clean doc tests all
