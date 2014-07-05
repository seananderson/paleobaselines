all: knit pdf rnwdrop

rnwdrop:
	cp risksupp.Rnw ~/Dropbox/nescent_extinction_map/risksupp.Rnw
	cp risksupp.pdf ~/Dropbox/nescent_extinction_map/risksupp.pdf
	#cp risksupp.rtf ~/Dropbox/nescent_extinction_map/risksupp.rtf

knit:
	Rscript -e "knitr::knit('risksupp.Rnw')"

pdf:
	pdflatex risksupp
	bibtex risksupp
	pdflatex risksupp
	pdflatex risksupp
	#pdflatex risksupp

dropbox:
	cp risksupp.pdf ~/Dropbox/Public/

sundia:
	cp risksupp.pdf ~/Dropbox/sundia/

clean:
	rm -r ../cache/*
	rm *.aux
	rm *.log
	rm *.bbl
	rm *.blg
	rm *.toc
	rm risksupp.tex
	#mkdir ../cache/

deleteranges:
	rm ../data/modern_and_paleo_ranges.rds

deletecv:
	rm ../data/val_test.rds

deleteroc:
	rm ../data/roc_vals.rds
	rm ../data/pred_oob.rds

deletecross:
	rm ../data/cross_pred.rds

deletestagemodels:
	rm ../data/stage_models.rds

deletemainmodel:
	rm ../data/main_model.rds

cleanall: clean deleteranges deletecv deleteroc deletecross deletestagemodels deletemainmodel

rtf:
	latex2rtf -Z2 -d 0 -b risksupp.bbl -E0 -M6 risksupp.tex
	#latex2rtf -d 0 -b risksupp.bbl -E0 -M6 risksupp.tex
	
