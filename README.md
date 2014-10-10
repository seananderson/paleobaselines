This (private) repository holds a mirror of the analysis files for the paper *Paleontological baselines for evaluating contemporary extinction risk in the coastal oceans* by Seth Finnegan, Sean C. Anderson, Paul G. Harnik, Carl Simpson, Derek P. Tittensor, Jarrett E. Byrnes, Zoe V. Finkel, David R. Lindberg, Lee Hsiang Liow, Rowan Lockwood, Heike K. Lotze, Craig M. McClain, Jenny L. McGuire, Aaron O'Dea, and John M. Pandolfi.

This work is a product of the Determinants of Extinction in Ancient and Modern Seas Working Group supported by the National Evolutionary Synthesis Center, NSF #EF-0905606.

The in-progress Supporting Material document (which also reproduces the analyses) can be found in the `analysis` folder.

An up-to-date version of the Supporting Materials document is available here:
<https://dl.dropboxusercontent.com/u/254940/risksupp.pdf>

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
