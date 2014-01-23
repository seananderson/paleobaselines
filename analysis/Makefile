all: pdf

pdf:
	Rscript -e "knitr::knit('risksupp.Rnw')"
	pdflatex risksupp
	bibtex risksupp
	pdflatex risksupp
	pdflatex risksupp
