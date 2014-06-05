all: pdf

pdf:
	Rscript -e "knitr::knit('risksupp.Rnw')"
	pdflatex risksupp
	cp risksupp.pdf ~/Dropbox/Public/

knit:
	Rscript -e "knitr::knit('risksupp.Rnw')"

tex:
	pdflatex risksupp
	cp risksupp.pdf ~/Dropbox/Public/

bib:
	pdflatex risksupp
	bibtex risksupp
	pdflatex risksupp
	pdflatex risksupp
	cp risksupp.pdf ~/Dropbox/Public/

clean:
	rm -rf ../cache/
	#rm -rf ../figs/
	rm *.aux
	rm *.log
	rm *.bbl
	rm *.blg
	rm *.toc
	rm risksupp.tex
	#mkdir ../figs/
	mkdir ../cache/

deleteranges:
	rm ../data/modern_and_paleo_ranges.rds

deletecv:
	rm ../data/val_test.rds
