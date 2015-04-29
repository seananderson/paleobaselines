## Paleontological baselines for evaluating extinction risk in the modern oceans

This repository contains the complete analysis for the paper:

Finnegan\*, S., S.C. Anderson\*, P.G. Harnik\*, C. Simpson, D.P. Tittensor, J.E. Byrnes, Z.V. Finkel, D.R. Lindberg, L.H. Liow, R. Lockwood, H.K. Lotze, C.M. McClain, J.L. McGuire, A. O’Dea, J.M. Pandolfi. Paleontological baselines for evaluating extinction risk in the modern oceans. In press at Science. (\*Authors contributed equally)

The analysis is written in the knitr file `analysis/risksupp.Rnw`, which generates the Supporting Materials document. 

### How to recreate the whole analysis

Clone the whole Git repository:

```
git clone https://github.com/seananderson/riskmaps.git
```

Or [download the .zip version](https://github.com/seananderson/riskmaps/archive/master.zip) and unzip it.

To recreate the analysis, you will first need the following R packages installed:

```R
install.packages(c("knitr", "devtools", paleorisk", "plyr", "reshape2",
  "ggplot2", "gbm", "maps", "mapproj", "maptools", "RColorBrewer", 
  "PBSmapping", "fields", "xtable", "lmodel2", "pROC", "rgeos", "dplyr", 
  "doParallel", "foreach", "grImport"))
```

Then in an R console run:

```R
knitr::knit("analysis/risksupp.Rnw")
```

Or, using the `makefile`, run the following on the command line:

```sh
cd analysis; make
```
