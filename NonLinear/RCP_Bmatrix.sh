#!/bin/bash
#SBATCH --partition=rajagopalan,stockle,cahnrs,cahnrs_bigmem,cahnrs_gpu,kamiak
#SBATCH --requeue
#SBATCH --job-name=R # Job Name
#SBATCH --output=R_%J.out
#SBATCH --error=R_%J.err
#SBATCH --time=0-04:00:00    # Wall clock time limit in Days-HH:MM:SS
#SBATCH --mem=200GB 
#SBATCH --nodes=1            # Node count required for the job
#SBATCH --ntasks-per-node=1  # Number of tasks to be launched per Node
#SBATCH --ntasks=1           # Number of tasks per array job
#SBATCH --cpus-per-task=1    # Number of threads per task (OMP threads)


echo
echo "--- We are now in $PWD, running an R script ..."
echo


echo "I am Slurm job ${SLURM_JOB_ID}, array job ${SLURM_ARRAY_JOB_ID}, and array task ${SLURM_ARRAY_TASK_ID}."

# Load R on compute node
module load r/4.1.0
Rscript --vanilla /weka/data/project/agaid/rajagopalan_agroecosystems/chaudhary/AnalogData_Sid/Creating_Variables_old/NonLinear/RCP_Bmatrix.R