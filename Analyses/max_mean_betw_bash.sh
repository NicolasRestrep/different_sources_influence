#!/bin/bash
#SBATCH --mem=4G # 4 GB RAM

module load R/4.0.3-rhel8

R CMD BATCH max_mean_betw_script.R