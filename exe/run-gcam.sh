#!/bin/zsh
#SBATCH -A im3
#SBATCH -t 10000
#SBATCH -N 1
job=$SLURM_JOB_NAME

# Load Modules
module purge
source  /etc/profile.d/modules.sh
module load git
module load svn/1.8.13
module load java/1.8.0_31
module load gcc/6.1.0

# Go to Folder
cd /pic/projects/im3/gcamusa/gcam-usa-im3/exe

echo 'Library config:'
ldd ./gcam.exe
date
time ./gcam.exe -Cconfiguration_usa_ssp3_rcp45.xml -Llog_conf.xml
date

