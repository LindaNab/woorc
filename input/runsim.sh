#!/bin/bash

#SBATCH --job-name=woorc          # Job name
#SBATCH --output=/exports/clinicalepi/Linda/woorc/job%A_scen_%a.out              # Output file name
#SBATCH --error=/exports/clinicalepi/Linda/woorc/job%A_scen_%a.err               # Error file name
#SBATCH --time=100:00:00                # Time limit
#SBATCH --nodes=1                       # Number of nodes
#SBATCH --ntasks-per-node=1             # MPI processes per node
#SBATCH --array=1

# Read in scenario number
scenario=${SLURM_ARRAY_TASK_ID}

module purge
module add statistical/R/4.0.2/gcc.8.3.1

Rscript --vanilla ./input/output.R 1000 $scenario "./output/"
