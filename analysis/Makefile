all: pdf

pdf:
	Rscript -e "knitr::knit('risksupp.Rnw')"
	xelatex risksupp
	cp risksupp.pdf ~/Dropbox/Public/

tex:
	xelatex risksupp

bib:
	xelatex risksupp
	bibtex risksupp
	xelatex risksupp
	xelatex risksupp

clean:
	rm -rf ../cache/
	rm -rf ../figs/
	rm *.aux
	rm *.log
	rm *.bbl
	rm *.blg
	rm *.toc
	rm risksupp.tex

