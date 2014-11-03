#!/bin/sh
#PBS -l walltime=04:00:00
#PBS -M sean_anderson@sfu.ca
#PBS -l pmem=5000mb

module load R/3.1.1
cd ~/riskmaps/analysis/
make knit
