#!/bin/bash

#SBATCH --job-name=simexvsmecor_logreg    # Job name
#SBATCH --output=/exports/clinicalepi/Linda/simexvsmecor/job%A_logreg_scen_%a.out              # Output file name
#SBATCH --error=/exports/clinicalepi/Linda/simexvsmecor/job%A_logreg_scen_%a.err               # Error file name
#SBATCH --time=100:00:00                 # Time limit
#SBATCH --nodes=1                       # Number of nodes
#SBATCH --ntasks-per-node=1             # MPI processes per node
#SBATCH --array=3-23

# Read in scenario number
scenario=${SLURM_ARRAY_TASK_ID}

module purge
module add statistical/R/4.0.2/gcc.8.3.1

Rscript --vanilla ./input/output_logreg.R 1000 $scenario "./output/"
