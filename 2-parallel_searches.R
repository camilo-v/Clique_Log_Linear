# ---------------------------------------------------------------------------------------------------------------------
#
#   OBJECTIVE:
#	The purpose of this program is to run several Markov chains in parallel. Each of the chains determine top graphs. 
#	The processIDs identify the chains.
#
#
#
#	AUTHORS:
#			Adrian Dobra (adobra@uw.edu)
#			Associate Professor of Statistics & Nursing
#			University of Washington
#
#			Camilo Valdes (cvalde03@fiu.edu)
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
# ---------------------------------------------------------------------------------------------------------------------

print( paste( "[", format(Sys.time(), "%m/%d/%y %H:%M:%S"),"] ", sep="") )
print( paste( "[", format(Sys.time(), "%m/%d/%y %H:%M:%S"),"] Stage 2... ", sep="") )
print( paste( "[", format(Sys.time(), "%m/%d/%y %H:%M:%S"),"] ", sep="") )

# ------------------------------------------------------ Input --------------------------------------------------------
#
#	Load the workspace from the previous run, "1-preprocess.R"
#
print( paste( "[", format(Sys.time(), "%m/%d/%y %H:%M:%S"),"] Loading Workspace Data...", sep="") )

workspaceDataFile_stage_1 = paste( outputDirBase, "/R_workspace/stage_1.RData", sep="" )
load( workspaceDataFile_stage_1 )

#	Read the data. The input file is a matrix with rows = samples and columns = variables
#	All variables must be categorical
positiveCounts = getPositiveCounts(matrixData)
biggestCounts = positiveCounts[order(positiveCounts[,ncol(positiveCounts)],decreasing=TRUE)[seq_len(nrow(positiveCounts))],]

gMaxMat = as.matrix(gMax)

# ------------------------------------------------ Parallel Searches --------------------------------------------------
#
#	Dispatch worker threads & processses to run sevelral Markov chains in 'parallel'.
#	The code below will S
#		1) Start up M ‘worker’ processes, and do any initialization needed on the workers.
#		2) Send any data required for each task to the workers
#		3) Split the task into M roughly equally-sized chunks, and send the chunks, including the R
#			code needed, to the workers
#		4) Wait for all the workers to complete their tasks, and ask them for their results.
#		5) Repeat the above as necessary.
#		6) Reap the worker processes at the end, i.e., shut them down.
#

# 	Also sets the number of random starting clique graphs.
processIDs = seq(from=1,to=100,by=1)

#	Size of the cluster should match the number of random clique graphs.
clusterSize = length(processIDs)

# 	The number of random starting clique graphs
nChainReplicates = clusterSize

print( paste( "[", format(Sys.time(), "%m/%d/%y %H:%M:%S"),"] Dispatching Threads (", clusterSize, ")...", sep="") )
	
#	Creates a parallel socket cluster.  The call below creates a set of copies of "R" running in parallel, and
#	communicating over sockets in the local machine/compute node.
cluster = makeCluster(clusterSize)

clusterSetRNGStream(cluster, 123)

results = clusterApply(cluster,processIDs,
                       function(processID,
						   		positiveCounts,
								biggestCounts,
								nChainIterations,
								matrixData,
								maxCliqueSize,
								gMaxMat,
								outputDirForGroup,
								source_code_directory)
	                       {
							   #	Each thread needs a copy of the source code.
							   source(paste(source_code_directory, "/classtable.R", sep=""))
							   source(paste(source_code_directory, "/classgraph.R", sep=""))
							   source(paste(source_code_directory, "/classmodel.R", sep=""))
							   source(paste(source_code_directory, "/markovchainfunctions.R", sep=""))
                     
							   runif(1000*processID)
						   
							   #
							   #	The main function for the BIC searches.
							   #	Located in 'markovchainfunctions.R'
							   #
							   markovChainSimpleChainGraph(	processID, 
								   							positiveCounts, 
															biggestCounts, 
															nChainIterations,
															matrixData,
															maxCliqueSize,
															gMaxMat,
															outputDirForGroup,
															source_code_directory)
							
							
							},	#	Anonimous function ends
                       positiveCounts,
					   biggestCounts,
					   nChainIterations,
					   matrixData,
					   maxCliqueSize,
					   gMaxMat,
					   outputDirForGroup,
					   source_code_directory )	# clusterApply() function ends


stopCluster(cluster)


#	Save the workspace
workspaceOutputFile_stage_2 = paste( outputDirBase, "/R_workspace/stage_2.RData", sep="" )
save( list=ls( all=TRUE ), file=workspaceOutputFile_stage_2)


# -------------------------------------------------- END Stage 2 ------------------------------------------------------

print( paste( "[", format(Sys.time(), "%m/%d/%y %H:%M:%S"),"] ", sep="") )
print( paste( "[", format(Sys.time(), "%m/%d/%y %H:%M:%S"),"] Stage 2, Done.", sep="") )
print( paste( "[", format(Sys.time(), "%m/%d/%y %H:%M:%S"),"] ", sep="") )

