#!/bin/bash
# Job name:
#SBATCH --job-name=summarize-classifications
#
# Quality of Service:
#SBATCH --qos=short
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
  simple-continuous/scripts/03a_summarize-classifications.R \
  simple-continuous/logs/03a_summarize-classifications.Rout
