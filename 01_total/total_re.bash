#!/bin/bash
#SBATCH --nodes=1
#SBATCH --ntasks=30
#SBATCH --mem=248gb
#SBATCH --time=48:00:00
#SBATCH --partition=std
#SBATCH --account=einhofja

module load R/4.4.0-gcc14.1.0

## copy job data
cp -r 0_functions.R /tmp
cp -r total_re.R /tmp
cp -r data_final.Rdata /tmp

## change to working director
cd /tmp

## do the work
Rscript total_re.R

## copy the results if they are required
cp -r /tmp/* /home/sozialwiss/einhofja/Dokumente/analysis_decomp