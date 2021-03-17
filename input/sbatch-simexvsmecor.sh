#!/bin/sh
#SBATCH -J simexvsmecorsimjob
#SBATCH -n 1
#SBATCH --time=01:00:00
#SBATCH --mem=1G
#SBATCH -o /exports/clinicalepi/Linda/simexvsmecor/out_err_messages/job%A_replicate%a.out # the %A is the job id, %a is SLURM_ARRAY_TASK_ID
#SBATCH -e /exports/clinicalepi/Linda/simexvsmecor/out_err_messages/job%A_replicate%a.err # the %A is the job id, %a is SLURM_ARRAY_TASK_ID
#SBATCH --array=1-4:1%3 # Read 'sequence 1 to 4 in steps of 1, of which only 3 jobs running at any same time'
​
# Read in scenario number
scenario=${SLURM_ARRAY_TASK_ID}
​
# Load module
module load statistical/R/4.0.2/gcc.8.3.1
​
# The below is how to batch
Rscript simexvsmecor_slurmscript.R 100 $scenario /exports/clinicalepi/Linda/simexvsmecor/simexvsmecor/output/
