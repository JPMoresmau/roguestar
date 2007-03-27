doc :
	(cd RSAGL && latex rsagl.tex && latex rsagl.tex && latex rsagl.tex && dvipdf rsagl.dvi)

clean :
	-rm -f RSAGL/*.dvi
	-rm -f RSAGL/*.aux
	-rm -f RSAGL/*.log
	-rm -f RSAGL/*.pdf
	-rm -f RSAGL/*.toc

.PHONY : clean doc
