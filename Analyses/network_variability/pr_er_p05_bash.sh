#!/bin/bash
#SBATCH --mem=4G # 4 GB RAM

module load R/4.0.3-rhel8

R CMD BATCH pr_er_p05_script.R