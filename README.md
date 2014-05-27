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

If you want to trash all the cached data files, run `make clean` and then run `make` again.

If you want to re-create the predictor data, delete the file `data/modern_and_paleo_ranges.rds` and re-run `make`. This file gets created the first time you run `make` and will take a long time (~10-20 minutes).

## Instructions for editing the SOM on GitHub

Find the file `risksupp.Rnw` in the folder `analysis`:

![](https://dl.dropboxusercontent.com/u/254940/riskmaps-wiki/risksupp-location.png)

Click on the `Edit` button:

![](https://dl.dropboxusercontent.com/u/254940/riskmaps-wiki/edit-button.png)

Enable the `Soft wrap` option:

![](https://dl.dropboxusercontent.com/u/254940/riskmaps-wiki/softwrap.png)

Now edit!

![](https://dl.dropboxusercontent.com/u/254940/riskmaps-wiki/regular-editing.png)

Or, you might find the "Zen Mode" easier to work in. Click this button:

![](https://dl.dropboxusercontent.com/u/254940/riskmaps-wiki/enter-zen-mode.png)

You'll now get a simplified editor that looks like this:

![](https://dl.dropboxusercontent.com/u/254940/riskmaps-wiki/zen-document.png)

When you're done, (first exit Zen Mode if you're in it) head to the bottom of the page, enter a short message describing your changes, and click the `Commit changes` button:

![](https://dl.dropboxusercontent.com/u/254940/riskmaps-wiki/commit-example.png)

It's best if you make a number of incremental edits and commits. This makes the version history easier to follow, reduces the chances you'll try and edit the same line as someone else at the same time, and reduces the chances of losing your work before saving. 

## All the LaTeX you need to know

Sections: `\section{A section name}`

Subsections: `\subsection{A subsection name}`

To cite something, probably the easiest option is to just leave a marker (like `REF Smith et al. 2012`) and I'll fix it up. 

If you want to do it yourself, export the citation from your reference manager as a BibTeX file, paste it into `risksupp.bib` and use the "citekey" (the text after `@article{`) as in `\cite{smith2012}`.

The stuff between `<<>>==` and `@` symbols is R code. You can safely ignore it.

Wrap URLs with `\url{}`.

Line breaks (i.e. hitting the Return key) get ignored unless there's a full blank line. Don't worry about this.

If you use a `%` symbol, you need to escape it with a backslash `\%` because `%` is the comment character in LaTeX.

Feel free to leave TODO markers where there's something to do.

Feel free to leave COMMENT markers where you have a comment. E.g. `COMMENT SA: should we worry about XX here?`.

Italics: `\textit{something}`

Bold: `\textbf{something}`
