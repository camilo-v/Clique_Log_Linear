# ---------------------------------------------------------------------------------------------------------------------
#
#   OBJECTIVE:
#	The purpose of this program is to pre-process a Matrix so that it can be used
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
print( paste( "[", format(Sys.time(), "%m/%d/%y %H:%M:%S"),"] Stage 1... ", sep="") )
print( paste( "[", format(Sys.time(), "%m/%d/%y %H:%M:%S"),"] ", sep="") )

# ------------------------------------------------------ Input --------------------------------------------------------
#
#	Read the data. The input file is a matrix with rows = samples, and columns = variables
#	All variables must be categorical.
matrixData = read.table( file=input_matrix_1, header=FALSE )

#	Matrix binarization (1 if it has a count, 0 if not)
#	Wound Microbiome ... non-zero numbers are binarized
print(head(matrixData, n=20))
matrixData[matrixData != 0] = 1
print(head(matrixData, n=20))

print( paste( "[", format(Sys.time(), "%m/%d/%y %H:%M:%S"),"] Positive Entries...", sep="") )

#	Determine which two-way marginals contain all positive entries
#	the edges of the graph that will eventually be determined are a subset
#	of the edges of this maximal graph. Each edge in the maximal graph
#	is associated with a two-way marginal with cell counts strictly positive

#	returns the cells that contain strictly positive counts in the full table
positiveCounts = getPositiveCounts(matrixData)

nVertices = ncol(matrixData)
gMax = Matrix(data=0,nrow=nVertices,ncol=nVertices)
myfreq = apply(matrixData,2,sum)

print( paste( "[", format(Sys.time(), "%m/%d/%y %H:%M:%S"),"] Number of Vertices: ", nVertices, sep="") )

for(i in seq_len(nVertices-1)) {
	
  for(j in (i+1):nVertices) {
	  
    cat("processing [",i,",",j,"]\n")
	
    if((0<myfreq[i])&(0<myfreq[j]))
    {
      x = createMarginalTable(c(i,j),matrixData)
      gMax[i,j] = gMax[j,i] = ifelse(allCountsPositive(x),1,0)
    }
  }
}

# -------------------------------------------------------- Output -----------------------------------------------------
	
outputFileForGraphPlot = paste( outputDirBase, "/", nChainIterations, "-gMax-plot.pdf", sep="" )
outputFileForBarGraph  = paste( outputDirBase, "/", nChainIterations, "-gMax-bars.pdf", sep="" )


# ------------------------------------------------------ Graph Plot ---------------------------------------------------
#
pdf( file=outputFileForGraphPlot )
huge.plot( gMax )
dev.off()

# ------------------------------------------------------- Bar Plot ----------------------------------------------------
#
pdf( file=outputFileForBarGraph )
barplot( apply( gMax, 2, sum ) / nrow( gMax ), xlab="Index", names.arg=as.character( seq_len( nrow( gMax ) ) ) )
dev.off()

#	Eliminate the inactive variables from the data
#	The marginal probabilities of these variables is very small so it's ok to eliminate them from the data
myvars = which(apply(gMax,2,sum)>0)

#	Delete the other columns from the data
matrixData = matrixData[,myvars]

#	Number of cells that have strictly positive counts
gMax = gMax[myvars,myvars]
diag(gMax) = 1

#	Determine the maximum size of a clique — this is given by the maximum number of matches for the same input
#	data point (read, sample count, etc.)
myfreq = apply(matrixData,1,sum)
maxCliqueSize = max(myfreq)

#	we should not search for cliques with more vertices than "maxCliqueSize"
#	maxCliqueSize = 6

#	Save the workspace, Stage 2 will pick-up from here.
workspaceOutputFile_stage_1 = paste( outputDirBase, "/R_workspace/stage_1.RData", sep="" )
save( list=ls( all=TRUE ), file=workspaceOutputFile_stage_1)


# -------------------------------------------------- END Stage 1 ------------------------------------------------------

print( paste( "[", format(Sys.time(), "%m/%d/%y %H:%M:%S"),"] ", sep="") )
print( paste( "[", format(Sys.time(), "%m/%d/%y %H:%M:%S"),"] Stage 1, Done.", sep="") )
print( paste( "[", format(Sys.time(), "%m/%d/%y %H:%M:%S"),"] ", sep="") )
