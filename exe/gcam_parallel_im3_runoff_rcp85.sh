#!/bin/sh
#SBATCH --partition=slurm,short,shared
#SBATCH --nodes=1
#SBATCH --time=10000
#SBATCH --job-name=lhs
#SBATCH -A im3
#SBATCH --array=0-3

# README -----------------------------------------------------------------------
#
# This script will launch SLURM tasks that will execute GCAM
# sbatch gcam_parallel_im3_runoff_rcp85.sh
#
# ------------------------------------------------------------------------------

# Load Modules
module purge
source  /etc/profile.d/modules.sh
module load git
module load svn/1.8.13
module load java/1.8.0_31
module load gcc/6.1.0

# Go to Folder
cd /pic/projects/im3/gcamusa/gcam-usa-im3/exe

# Files to run in parallel
FILES=('configuration_rcp85hotter_ssp3_rcp85gdp_runoff.xml' 'configuration_rcp85cooler_ssp3_rcp85gdp_runoff.xml' 'configuration_rcp85cooler_ssp5_runoff.xml' 'configuration_rcp85hotter_ssp5_runoff.xml')

CONFIG=${FILES[$SLURM_ARRAY_TASK_ID]}

echo "Run ID: ${SLURM_ARRAY_TASK_ID}"
echo "FILES:  ${FILES[@]}"
echo "CONFIG:  ${CONFIG}"

date
time ./gcam.exe -C$CONFIG -Llog_conf.xml
date

echo 'completed'

