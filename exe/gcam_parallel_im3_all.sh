#!/bin/sh
#SBATCH --partition=slurm,shared
#SBATCH --nodes=1
#SBATCH --time=10000
#SBATCH --job-name=lhs
#SBATCH -A im3
#SBATCH --array=0-54

# README -----------------------------------------------------------------------
#
# This script will launch SLURM tasks that will execute GCAM
# sbatch gcam_parallel_im3_all.sh
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
FILES=(
# all impacts with assumptions 1 2 and 3
'configuration_rcp45cooler_ssp3_im3.xml'
'configuration_rcp45cooler_ssp3_im3_allow_new_nuc.xml'
'configuration_rcp45cooler_ssp3_im3_base_nuc_only.xml'
'configuration_rcp45cooler_ssp5_im3.xml'
'configuration_rcp45cooler_ssp5_im3_allow_new_nuc.xml'
'configuration_rcp45cooler_ssp5_im3_base_nuc_only.xml'
'configuration_rcp45hotter_ssp3_im3.xml'
'configuration_rcp45hotter_ssp3_im3_allow_new_nuc.xml'
'configuration_rcp45hotter_ssp3_im3_base_nuc_only.xml'
'configuration_rcp45hotter_ssp5_im3.xml'
'configuration_rcp45hotter_ssp5_im3_allow_new_nuc.xml'
'configuration_rcp45hotter_ssp5_im3_base_nuc_only.xml'
'configuration_rcp85cooler_ssp3_rcp85gdp_im3.xml'
'configuration_rcp85cooler_ssp3_rcp85gdp_im3_allow_new_nuc.xml'
'configuration_rcp85cooler_ssp3_rcp85gdp_im3_base_nuc_only.xml'
'configuration_rcp85cooler_ssp5_im3.xml'
'configuration_rcp85cooler_ssp5_im3_allow_new_nuc.xml'
'configuration_rcp85cooler_ssp5_im3_base_nuc_only.xml'
'configuration_rcp85hotter_ssp3_rcp85gdp_im3.xml'
'configuration_rcp85hotter_ssp3_rcp85gdp_im3_allow_new_nuc.xml'
'configuration_rcp85hotter_ssp3_rcp85gdp_im3_base_nuc_only.xml'
'configuration_rcp85hotter_ssp5_im3.xml'
'configuration_rcp85hotter_ssp5_im3_allow_new_nuc.xml'
'configuration_rcp85hotter_ssp5_im3_base_nuc_only.xml'

# agyield hdcd and runoff with nuclear moratorium addon as new baseline
'configuration_rcp45cooler_ssp3_agyields.xml'
'configuration_rcp45cooler_ssp3_hdcd.xml'
'configuration_rcp45cooler_ssp3_runoff.xml'
'configuration_rcp45cooler_ssp5_agyields.xml'
'configuration_rcp45cooler_ssp5_hdcd.xml'
'configuration_rcp45cooler_ssp5_runoff.xml'
'configuration_rcp45hotter_ssp3_agyields.xml'
'configuration_rcp45hotter_ssp3_hdcd.xml'
'configuration_rcp45hotter_ssp3_runoff.xml'
'configuration_rcp45hotter_ssp5_agyields.xml'
'configuration_rcp45hotter_ssp5_hdcd.xml'
'configuration_rcp45hotter_ssp5_runoff.xml'
'configuration_rcp85cooler_ssp3_rcp85gdp_agyields.xml'
'configuration_rcp85cooler_ssp3_rcp85gdp_hdcd.xml'
'configuration_rcp85cooler_ssp3_rcp85gdp_runoff.xml'
'configuration_rcp85cooler_ssp5_agyields.xml'
'configuration_rcp85cooler_ssp5_hdcd.xml'
'configuration_rcp85cooler_ssp5_runoff.xml'
'configuration_rcp85hotter_ssp3_rcp85gdp_agyields.xml'
'configuration_rcp85hotter_ssp3_rcp85gdp_hdcd.xml'
'configuration_rcp85hotter_ssp3_rcp85gdp_runoff.xml'
'configuration_rcp85hotter_ssp5_agyields.xml'
'configuration_rcp85hotter_ssp5_hdcd.xml'
'configuration_rcp85hotter_ssp5_runoff.xml'

# ssp with nuclear moratorium addon as new baseline
'configuration_usa_ssp2.xml'
'configuration_usa_ssp2_rcp45.xml'
'configuration_usa_ssp3.xml'
'configuration_usa_ssp3_rcp45.xml'
'configuration_usa_ssp3_rcp85gdp.xml'
'configuration_usa_ssp5.xml'
'configuration_usa_ssp5_rcp45.xml'
)

CONFIG=${FILES[$SLURM_ARRAY_TASK_ID]}

echo "Run ID: ${SLURM_ARRAY_TASK_ID}"
echo "FILES:  ${FILES[@]}"
echo "CONFIG:  ${CONFIG}"

date
time ./gcam.exe -C$CONFIG -Llog_conf.xml
date

echo 'completed'

