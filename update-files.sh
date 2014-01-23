#!/usr/bin/env bash

#rsync -rav --delete --include '*.R$' --include '*.r$' --exclude '*' ~/Dropbox/nescent_extinction_map/r/ ~/src/riskmaps/r/
rm r/*.r
rm r/*.R
cp ~/Dropbox/nescent_extinction_map/r/*.r r/
cp ~/Dropbox/nescent_extinction_map/r/*.R r/
cp ~/Dropbox/nescent_extinction_map/r/*.Rnw r/
cp ~/Dropbox/nescent_extinction_map/r/*.bib r/

rm r/*conflicted\ copy*
