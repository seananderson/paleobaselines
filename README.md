### Paleontological baselines for evaluating extinction risk in the modern oceans

This repository contains the complete analysis for the paper:

Finnegan\*, S., S.C. Anderson\*, P.G. Harnik\*, C. Simpson, D.P. Tittensor, J.E. Byrnes, Z.V. Finkel, D.R. Lindberg, L.H. Liow, R. Lockwood, H.K. Lotze, C.M. McClain, J.L. McGuire, A. O’Dea, J.M. Pandolfi. Paleontological baselines for evaluating extinction risk in the modern oceans. In press at Science. (\*Authors contributed equally)

The analysis is writen in a knitr document `analysis/risksupp.Rnw` that generates the Supporting Materials document. 

## How to recreate the whole analysis

Clone the whole Git repository:

```
git clone https://github.com/seananderson/riskmaps.git
```

Or [download the .zip version](https://github.com/seananderson/riskmaps/archive/master.zip) and unzip it.

Install the paleorisk R package in an R console:

```
# setwd() to this root folder, then:
install.packages("paleorisk", type = "source", repos = NULL)
```

Then, on the command line `cd` to the `analysis` folder and run `make`.

If you want to trash all the cached data files, run `make clean` and then run `make` again. In fact, any time you've made semi-major changes, you should run `make clean` before running `make`. knitr tries to cache chunks intelligently, but for complicated dependencies it doesn't always get it right.

If you want to re-create the predictor data, delete the file `data/modern_and_paleo_ranges.rds` and re-run `make`. This file gets created the first time you run `make` and will take a long time (~10-20 minutes).
