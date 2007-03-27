all : doc tests

doc :
	(cd RSAGL && latex rsagl.tex && latex rsagl.tex && latex rsagl.tex && dvipdf rsagl.dvi)
	cp RSAGL/rsagl.pdf ./rsagl.pdf

tests :
	ghc --make RSAGL/Tests.hs -main-is RSAGL.Tests.main -o rsagl-tests

clean :
	-rm -f RSAGL/*.dvi
	-rm -f RSAGL/*.aux
	-rm -f RSAGL/*.log
	-rm -f RSAGL/*.pdf
	-rm -f RSAGL/*.toc
	-rm -f RSAGL/*.hi
	-rm -f RSAGL/*.o
	-rm -f rsagl-tests
	-rm -f rsagl.pdf

.PHONY : clean doc tests all
