#!/bin/bash
# Job name:
#SBATCH --job-name=apply-aug-mod-cov-method
#
# Quality of Service:
#SBATCH --qos=medium
#
# Processors (1 node = 28 cores):
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=10
#SBATCH --mem-per-cpu=16G
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
  realistic-continuous/scripts/02c_apply-augmented-modified-covariates-method.R \
  realistic-continuous/logs/02c_apply-augmented-modified-covariates-method.Rout
