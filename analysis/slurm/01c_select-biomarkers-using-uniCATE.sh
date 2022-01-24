#!/bin/bash
# Job name:
#SBATCH --job-name=semiparametric-gene-selection
#
# Quality of Service:
#SBATCH --qos=short
#
# Processors (1 node = 28 cores):
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=5
#
# Mail type:
#SBATCH --mail-type=all
#
# Mail user:
#SBATCH --mail-user=boileap2@gene.com
#
# Job output:
#SBATCH --output=slurm.out
#SBATCH --error=slurm.out
#
## Command(s) to run:
## Initialize work environment like
cd ~/uniCATE-application-real-data
module load R/prd
#
## Run the script
R CMD BATCH --no-save --no-restore \
  R/scripts/01c_select-biomarkers-using-uniCATE.R \
  logs/01c_select-biomarkers-using-uniCATE.Rout
