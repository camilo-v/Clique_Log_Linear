#
# ---------------------------------------------------------------------------------------------------------------------
#
#	OBJECTIVE:
#	The purpose of this script is to serve as the main driver for the preprocess, parallel, and post-process stages
#	of the log-linear models method
#
#
#   NOTES:
#   Please see the dependencies section below for the required libraries (if any).
#
#   DEPENDENCIES:
#
#		• optparse
#		• Cairo
#       • sna
#		• huge
#		• pheatmap
#		• parallel
#		• stringr
#		• Matrix
#		• igraph
#
#   AUTHOR:	Camilo Valdes (cvalde03@fiu.edu)
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


library(Cairo)	# Requires X11 on OS X
library(sna)
library(huge)
library(pheatmap)
library(optparse)

options(width=220, digits=15, warn=1, echo=FALSE) # Enable printing of large numbers. R defaults to "rounding" for display.


# --------------------------------------------- Command-line Arguements -----------------------------------------------

option_list = list(
	make_option(c("-p", "--project_dir"), type="character", default=NULL, help="path to project directory", metavar="character"),
	make_option(c("-s", "--source_code"), type="character", default=NULL, help="path to source code", metavar="character"),
	make_option(c("-m1", "--matrix_1"), type="character", default=NULL, help="path to input matrix 1", metavar="character"),
	make_option(c("-m2", "--matrix_2"), type="character", default=NULL, help="path to input matrix 2", metavar="character"),
	make_option(c("-c", "--chain_iterations"), type="integer", default=100, help="number of chain iterations", metavar="integer")
); 
 
opt_parser = OptionParser(option_list=option_list);
opt = parse_args(opt_parser);

#
#	Check that we have the command line parameters we need
#
if ( is.null(opt$project_dir) || is.null(opt$matrix_1) || is.null(opt$matrix_2) || is.null(opt$chain_iterations) ) {
				
			print_help(opt_parser)
			stop("Missing files!.n", call.=FALSE)
}

# ------------------------------------------------- Project Setup -----------------------------------------------------

print( paste( "[", format(Sys.time(), "%m/%d/%y %H:%M:%S"),"] ", sep="") )
print( paste( "[", format(Sys.time(), "%m/%d/%y %H:%M:%S"),"] Analysis starting... ", sep="") )
print( paste( "[", format(Sys.time(), "%m/%d/%y %H:%M:%S"),"] ", sep="") )

#	Main source code repository.
source_code_directory = opt$source_code

#	Load the libraries we need from the log-linear project
source(paste(source_code_directory, "/classtable.R", sep=""))
source(paste(source_code_directory, "/classgraph.R", sep=""))
source(paste(source_code_directory, "/classmodel.R", sep=""))
source(paste(source_code_directory, "/graphToDot.R", sep=""))
source(paste(source_code_directory, "/markovchainfunctions.R", sep=""))

#	Variables for 'this' run
project_directory	= opt$project_dir
input_matrix_1		= opt$matrix_1
input_matrix_2		= opt$matrix_2

# ------------------------------------------------ Chain Iterations ---------------------------------------------------
#
#	The number of Chain Iterations to run for each random clique.  Tests are based on this number, and so it also 
#	defines the output folder structure.
#	Original from Adrian's version was 100,000
#
nChainIterations = opt$chain_iterations


# ----------------------------------------------- Output Directories --------------------------------------------------
#
#	We'll define the output directory for 'this' run.  All output and workspace data are saved in this directory
outputDirBase = paste(project_directory, '/output/', nChainIterations, sep="")
dir.create( file.path( outputDirBase ), showWarnings=FALSE, recursive=TRUE )
outputDirForGroup = paste( outputDirBase, sep="" )


#	We save the workspace data from each run so that we can recover in the event of an error.
dir.create( file.path( outputDirBase, "R_workspace" ), showWarnings=FALSE, recursive=TRUE )

#	We save the workspace data from each run so that we can recover in the event of an error.
dir.create( file.path( outputDirBase, "R_data" ), showWarnings=FALSE, recursive=TRUE )





# ---------------------------------------------- Stage 1, Pre-Process -------------------------------------------------
#
#	See "1-preprocess.R" for details on the preprocessing steps.
#
source(paste( source_code_directory, "/1-preprocess.R", sep=""))


# ------------------------------------------ Stage 2, Parallel BIC Searches -------------------------------------------
#
#	See "2-parallel_searches.R" for details on the preprocessing steps.
#
source(paste( source_code_directory, "/2-parallel_searches.R", sep=""))


# --------------------------------------------- Stage 3, Post-Process -------------------------------------------------
#
#	See "3-postprocess.R" for details on the preprocessing steps.
#
source(paste( source_code_directory, "/3-postprocess.R", sep=""))




# ----------------------------------------------- Output Directories --------------------------------------------------
#
#	FIN.
#
print( paste( "[", format(Sys.time(), "%m/%d/%y %H:%M:%S"),"] ", sep="") )
print( paste( "[", format(Sys.time(), "%m/%d/%y %H:%M:%S"),"] Analysis Done.", sep="") )
print( paste( "[", format(Sys.time(), "%m/%d/%y %H:%M:%S"),"] ", sep="") )

