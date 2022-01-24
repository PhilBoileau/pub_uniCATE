#!/bin/bash
# Job name:
#SBATCH --job-name=unicate-bias-variance
#
# Quality of Service:
#SBATCH --qos=veryshort
#
# Processors (1 node = 28 cores):
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#
# Mail type:
#SBATCH --mail-type=all
#
# Mail user:
#SBATCH --mail-user=philippe.boileau@roche.com
#
# Job output:
#SBATCH --output=slurm.out
#SBATCH --error=slurm.out
#
## Command(s) to run:
## Initialize work environment like
cd ~/HD-biomarker-discovery/simulations
module load R/prd
#
## Run the script
R CMD BATCH --no-save --no-restore \
  simple-continuous/scripts/03b_bias-and-variance.R \
  simple-continuous/logs/03b_bias-and-variance.Rout
