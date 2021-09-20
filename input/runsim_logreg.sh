#!/bin/bash

#SBATCH --job-name=simexvsmecor_logreg    # Job name
#SBATCH --output=/exports/clinicalepi/Linda/simexvsmecor/job%A_logreg2_scen_%a.out              # Output file name
#SBATCH --error=/exports/clinicalepi/Linda/simexvsmecor/job%A_logreg2_scen_%a.err               # Error file name
#SBATCH --time=100:00:00                 # Time limit
#SBATCH --nodes=1                       # Number of nodes
#SBATCH --ntasks-per-node=1             # MPI processes per node
#SBATCH --array=1,2,3,4,5,6,7,8,9,10,11,12,14,15,16,17,18,19,20,21,22,23

# Read in scenario number
scenario=${SLURM_ARRAY_TASK_ID}

module purge
module add statistical/R/4.0.2/gcc.8.3.1

Rscript --vanilla ./input/output_logreg.R 1000 $scenario "./output/"
