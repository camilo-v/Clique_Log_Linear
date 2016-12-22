#!/bin/bash

# ---------------------------------------------------------------------------------------------------------------------
#
#	OBJECTIVE:
#	The purpose of this script is to carry out a test for the log-linear models code.
#
#   NOTES:
#   Please see the dependencies and/or assertions section below for any requirements.
#
#   DEPENDENCIES:
#
#       • R
#
#	AUTHOR:	Camilo Valdes (cvalde03@fiu.edu)
#			School of Computing and Information Sciences, 
#			Florida International University (FIU)
#
#	COLLABORATORS:
#			
#			Jennifer Clarke (jclarke3@unl.edu)
#			Department of Food Science and Technology
#			Department of Statistics, 
#			University of Nebraska, Lincoln
#
#			Bertrand Clarke (bclarke3@unl.edu)
#			Department of Statistics, 
#			University of Nebraska, Lincoln
#
#			Adrian Dobra (adobra@uw.edu)
#			Associate Professor of Statistics & Nursing
#			University of Washington
#
# ---------------------------------------------------------------------------------------------------------------------



SOURCE_CODE_DIR='/Users/camilo/Documents/CCS/Jennifer/Projects/Bacterial_Metagenomics-Adrian/Simulations_3/source_code_sims'

PROJECT_DIR='/Users/camilo/Documents/CCS/Jennifer/Projects/Bacterial_Metagenomics-Adrian/Simulations_3/'
INPUT_MATRIX_1=$PROJECT_DIR'/matrices/sim-matrix_1.txt'
INPUT_MATRIX_2=$PROJECT_DIR'/matrices/sim-matrix_1.txt'

CHAIN_ITERATIONS=10000

echo ""
echo "[" `date '+%m/%d/%y %H:%M:%S'` "]"
echo "[" `date '+%m/%d/%y %H:%M:%S'` "] Starting Test..."
echo "[" `date '+%m/%d/%y %H:%M:%S'` "] "$TESTING_GROUP_NAME" - "$CHAIN_ITERATIONS
echo "[" `date '+%m/%d/%y %H:%M:%S'` "]"
echo "[" `date '+%m/%d/%y %H:%M:%S'` "]"

Rscript $SOURCE_CODE_DIR/_mainDriver.R 	--project_dir $PROJECT_DIR \
										--source_code $SOURCE_CODE_DIR \
										--matrix_1 $INPUT_MATRIX_1 \
										--matrix_2 $INPUT_MATRIX_2 \
										--chain_iterations $CHAIN_ITERATIONS


echo "[" `date '+%m/%d/%y %H:%M:%S'` "]"
echo "[" `date '+%m/%d/%y %H:%M:%S'` "] Test finished."
echo ""