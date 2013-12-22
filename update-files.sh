#!/usr/bin/env bash

rsync -rav --delete --include '*.R$' --include '*.r$' --exclude '*' ~/Dropbox/nescent_extinction_map/r/ r/
#rm r/*conflicted\ copy*
