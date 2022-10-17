#!/bin/bash
#SBATCH --partition=rajagopalan,stockle,cahnrs,cahnrs_bigmem,cahnrs_gpu,kamiak
#SBATCH --requeue
#SBATCH --job-name=R # Job Name
#SBATCH --output=R_%A_%a.out
#SBATCH --error=R_%A_%a.err
#SBATCH --time=0-40:00:00    # Wall clock time limit in Days-HH:MM:SS
#SBATCH --mem=10GB 
#SBATCH --nodes=1            # Node count required for the job
#SBATCH --ntasks-per-node=1  # Number of tasks to be launched per Node
#SBATCH --ntasks=1           # Number of tasks per array job
#SBATCH --cpus-per-task=1    # Number of threads per task (OMP threads)
#SBATCH --array=0-20000

skip=10
start_id=$(($SLURM_ARRAY_TASK_ID*$skip))
end_id=$((${start_id}+$skip-1))
echo "for ${SLURM_ARRAY_TASK_ID} range is: ${start_id},${end_id}"

for (( fid=${start_id}; fid<=${end_id}; fid++ )); do
module load r/4.1.0
cd /data/project/agaid/rajagopalan_agroecosystems/chaudhary/AnalogData_Sid/Creating_Variables_old/NonLinear/
Rscript --vanilla ./script_model_nonlinear.R ${fid}

done