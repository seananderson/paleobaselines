all: pdf

pdf:
	Rscript -e "knitr::knit('risksupp.Rnw')"
	xelatex risksupp
	bibtex risksupp
	xelatex risksupp
	xelatex risksupp
	cp risksupp.pdf ~/Dropbox/Public/

clean:
	rm -rf ../cache/
	rm -rf ../figs/
	rm *.aux
	rm *.log
	rm *.bbl
	rm *.blg
	rm *.toc
	rm risksupp.tex

