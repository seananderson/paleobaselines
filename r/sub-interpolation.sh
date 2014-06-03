#!/bin/sh
#PBS -l walltime=20:00:00
#PBS -M sean_anderson@sfu.ca
#PBS -l mem=2000mb

R CMD BATCH --no-save /home/anderson/riskmaps/r/README_interpolation.R
