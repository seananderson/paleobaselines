# Paleontological baselines for evaluating extinction risk in the modern oceans

This repository contains the complete analysis for the paper:

Finnegan\*, S., S.C. Anderson\*, P.G. Harnik\*, C. Simpson, D.P. Tittensor, J.E. Byrnes, Z.V. Finkel, D.R. Lindberg, L.H. Liow, R. Lockwood, H.K. Lotze, C.M. McClain, J.L. McGuire, A. O’Dea, J.M. Pandolfi. Paleontological baselines for evaluating extinction risk in the modern oceans. In press at Science. (\*Authors contributed equally)

The analysis is written in the knitr file `analysis/risksupp.Rnw`, which generates the Supporting Materials document and all figures.

To recreate the analysis, first clone the Git repository:

```
git clone https://github.com/seananderson/paleobaselines.git
```

Or [download the .zip version](https://github.com/seananderson/paleobaselines/archive/master.zip) and unzip it.

You will need the following R packages installed:

```R
x <- c("knitr", "devtools", "plyr", "reshape2", "ggplot2", "gbm", "maps",
  "mapproj", "maptools", "RColorBrewer", "PBSmapping", "fields", "xtable",
  "lmodel2", "pROC", "rgeos", "dplyr", "doParallel", "foreach", "grImport")
install.packages(x)
```

Then in an R console (version 3.2.0 or higher) run:

```R
setwd("analysis")
knitr::knit("risksupp.Rnw")
```

Or, using the `makefile`, run the following on the command line:

```sh
cd analysis; make
```
