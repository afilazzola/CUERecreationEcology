#!/bin/bash
#SBATCH --account=def-sapna # specify account
#SBATCH --time=02:59:00      # time for operation to run 
#SBATCH --mem-per-cpu=2G    ## specify memory for operation
#SBATCH --cpus-per-task=1   # Specify processors
#SBATCH --mail-user=alex.filazzola@outlook.com   ## specify email for notification
#SBATCH --mail-type=BEGIN
#SBATCH --mail-type=END
#SBATCH --mail-type=FAIL
#SBATCH --job-name=MapboxMatch
#SBATCH --error=MapboxMatch.%J_%a.stdout
#SBATCH --output=MapboxMatch.%J_%a.stderr
#SBATCH --array=0-100

## Export environment
parallel --record-env

## specify array ID
IDX=$(( SLURM_ARRAY_TASK_ID))

## Load modules
module load StdEnv/2020  gcc/9.3.0 r-bundle-bioconductor/3.12
module load netcdf
module load udunits
module load r/4.1.0
module load grass

## Run  
Rscript ~/projects/def-sapna/afila/CUERecEcology/scripts/MapboxMatch.r ${IDX} 

