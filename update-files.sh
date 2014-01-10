#!/usr/bin/env bash

#rsync -rav --delete --include '*.R$' --include '*.r$' --exclude '*' ~/Dropbox/nescent_extinction_map/r/ ~/src/riskmaps/r/
cp ~/Dropbox/nescent_extinction_map/r/*.r r/
cp ~/Dropbox/nescent_extinction_map/r/*.R r/

rm r/*conflicted\ copy*
