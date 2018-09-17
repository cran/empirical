default:
	make empirical.pdf
	make removeother

empirical.pdf: empirical.tex
	pdflatex empirical.tex

removeother:
	rm -f *.tex
	rm -f *.aux
	rm -f *.log
	rm -f temp*
	rm -f Rplots*
