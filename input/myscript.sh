#!/bin/bash

#SBATCH --job-name=simexvsmecor          # Job name
#SBATCH --output=/exports/clinicalepi/Linda/simexvsmecor/job%A_scen%a.out              # Output file name
#SBATCH --error=/exports/clinicalepi/Linda/simexvsmecor/job%A_scen%a.err               # Error file name
#SBATCH --time=03:45:00                 # Time limit
#SBATCH --nodes=1                       # Number of nodes
#SBATCH --ntasks-per-node=1             # MPI processes per node
#SBATCH --array=1-14

# Read in scenario number
scenario=${SLURM_ARRAY_TASK_ID} 

module purge
module add statistical/R/4.0.2/gcc.8.3.1

Rscript --vanilla ./input/simexvsmecor_slurmscript.R 5000 $scenario "./output/"
