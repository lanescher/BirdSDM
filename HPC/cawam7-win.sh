#!/bin/bash
 
#SBATCH --job-name=fiona
#SBATCH --nodes=1
#SBATCH --cpus-per-task=3
#SBATCH --mem-per-cpu=30000
#SBATCH --hint=nomultithread
#SBATCH --partition=cpu
#SBATCH --account=eesc
#SBATCH --time=2-00:00:00
#SBATCH --mail-type=ALL
#SBATCH --mail-user=rmummah@usgs.gov
#SBATCH -o fiona/cawam7-%j.out
#SBATCH -e fiona/cawam7-%j.out


## Load modules (you can see options using 'module avail')
module load jags/4.3.1 cray-R/4.2.1.2

# TO RENAME WITHOUT ARRAY
## dummy-%j.out  # %j = job ID
## dummy-%j.err

## Good to start by sleeping for 5 minutes 
## Typically if you run too many short jobs, you get kicked off.
# echo "Now sleeping"
# sleep 5m



## Set the name of the R script to run, and the directory in which to save outputs
script=/caldera/hovenweep/projects/usgs/ecosystems/eesc/rmummah/proj05-fiona/code/cawa7.R


# run your script:
## Pass the name of the script to run as a variable called $script
## $SLURM_ARRAY_TASK_ID is a variable automatically generated by the bash input 
## in line 4. This script will basically call the Rscript line below as a 
## for loop with each possible $SLURM_ARRAY_TASK_ID as an input. Each 
## SLURM_ARRAY_TASK_ID process runs on its own processor.

srun --cpu-bind=none Rscript $script $SLURM_ARRAY_TASK_ID
