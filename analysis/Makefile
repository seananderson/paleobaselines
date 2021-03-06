all: knit pdf

rnwdrop:
	cp risksupp.Rnw ~/Dropbox/nescent_extinction_map/risksupp.Rnw
	cp risksupp.pdf ~/Dropbox/nescent_extinction_map/risksupp.pdf

knit:
	Rscript -e "knitr::knit('risksupp.Rnw')"

pdf:
	pdflatex risksupp
	bibtex risksupp
	pdflatex risksupp
	pdflatex risksupp
	pdflatex risksupp

clean:
	rm -rf ../cache/*
	rm -f *.aux
	rm -f *.log
	rm -f *.bbl
	rm -f *.blg
	rm -f *.toc
	rm -f risksupp.tex

deleteranges:
	rm ../data/modern_and_paleo_ranges.rds

deletecv:
	rm -f ../data/val_test.rds
	rm -f ../data/val_test_1.0.rds
	rm -f ../data/val_test_sing.rds
	rm -f ../data/val_test_singleton.rds

deleteroc:
	rm -f ../data/roc_vals.rds
	rm -f ../data/pred_oob.rds

deletecross:
	rm -f ../data/cross_pred.rds

deletestagemodels:
	rm -f ../data/stage_models.rds

deletemainmodel:
	rm -f ../data/main_model.rds
	rm -f ../data/main_model_1.0.rds
	rm -f ../data/main_model_sing.rds

deleterest:
	rm -f ../data/stand-predictors-cen-obis.rds
	rm -f ../data/by.prov.classes.rda
	rm -f ../data/by.prov.all.jk.rda
	rm -f ../data/by.prov.all.no.singles.rda
	rm -f ../data/by.prov.all.obis.culls.rda
	rm -f ../data/by.prov.classes.no.singles.rda
	rm -f ../data/by.prov.classes.obis.culls.rda

cachedropbox:
	cp ../data/val_test.rds ~/Dropbox/riskmaps-cache/
	cp ../data/val_test_1.0.rds ~/Dropbox/riskmaps-cache/
	cp ../data/val_test_sing.rds ~/Dropbox/riskmaps-cache/
	cp ../data/val_test_singleton.rds ~/Dropbox/riskmaps-cache/
	cp ../data/roc_vals.rds ~/Dropbox/riskmaps-cache/
	cp ../data/pred_oob.rds ~/Dropbox/riskmaps-cache/
	cp ../data/cross_pred.rds ~/Dropbox/riskmaps-cache/
	cp ../data/stage_models.rds ~/Dropbox/riskmaps-cache/
	cp ../data/main_model.rds ~/Dropbox/riskmaps-cache/
	cp ../data/main_model_1.0.rds ~/Dropbox/riskmaps-cache/
	cp ../data/main_model_sing.rds ~/Dropbox/riskmaps-cache/
	cp ../data/stand-predictors-cen-obis.rds ~/Dropbox/riskmaps-cache/
	cp ../data/modern_and_paleo_ranges.rds ~/Dropbox/riskmaps-cache/

# full
cleanall: clean deleteranges deletecv deleteroc deletecross deletestagemodels deletemainmodel deleterest

# partial for speed:
cleanall2: clean deletemainmodel deleterest

rtf:
	latex2rtf -Z2 -d 0 -b risksupp.bbl -E0 -M6 risksupp.tex
	
png:
	#rm ../figs/*\.png
	#rm ../static-figs/*\.png
	cp risksupp.tex risksupp-png.tex
	Rscript -e "source('pdf2png.R')"
	perl -p -i -e "s/\.pdf/.png/g" risksupp-png.tex
	perl -p -i -e "s/Cover-page.png/Cover-page.pdf/g" risksupp-png.tex
	awk -v n=2 -v s="\\\DeclareGraphicsExtensions{.png,.pdf}" 'NR == n {print s} {print}' risksupp-png.tex > risksupp-png2.tex
	mv risksupp-png2.tex risksupp-png.tex
	pdflatex risksupp-png.tex
	bibtex risksupp-png.tex
	pdflatex risksupp-png.tex
	pdflatex risksupp-png.tex
	pdflatex risksupp-png.tex

mainfigsdropbox:
	cp ../figs/partial-plot-base.pdf ~/Dropbox/nescent_extinction_map/fig1.pdf
	cp ../figs/class-risk-maps-mean-log-ext.pdf ~/Dropbox/nescent_extinction_map/fig2.pdf
	cp ../figs/hotspots.pdf ~/Dropbox/nescent_extinction_map/fig3.pdf

splicecover:
	gs \
	-o paleobaselines-som.pdf \
  -dDEVICEWIDTHPOINTS=612  \
  -dDEVICEHEIGHTPOINTS=792 \
  -dPDFFitPage \
	-sDEVICE=pdfwrite \
	-dPDFSETTINGS=/prepress \
   Cover-page.pdf \
	 risksupp.pdf
