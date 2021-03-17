#!/bin/bash

#SBATCH --job-name=HelloWord            # Job name
#SBATCH --output=slurm.out              # Output file name
#SBATCH --error=slurm.err               # Error file name
#SBATCH --partition=short               # Partition
#SBATCH --time=00:05:00                 # Time limit
#SBATCH --nodes=1                       # Number of nodes
#SBATCH --ntasks-per-node=1             # MPI processes per node

module purge
module add statistical/R/4.0.2/gcc.8.3.1

Rscript --vanilla HelloWorld.R
