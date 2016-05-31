# ---------------------------------------------------------------------------------------------------------------------
#
#   OBJECTIVE:
#	The purpose of this program is to process the output data from the "2-parallel_searches.R" script.  It processes the
#	output of the Markov chains.
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
print( paste( "[", format(Sys.time(), "%m/%d/%y %H:%M:%S"),"] Stage 3... ", sep="") )
print( paste( "[", format(Sys.time(), "%m/%d/%y %H:%M:%S"),"] ", sep="") )

# ------------------------------------------------------ Input --------------------------------------------------------
#
#	Load the workspace from the previous run, "1-preprocess.R" and "2-parallel_searches.R"
#
print( paste( "[", format(Sys.time(), "%m/%d/%y %H:%M:%S"),"] Loading Workspace Data...", sep="") )
workspaceDataFile_stage_1 = paste( outputDirBase, "/R_workspace/stage_1.RData", sep="" )
workspaceDataFile_stage_2 = paste( outputDirBase, "/R_workspace/stage_2.RData", sep="" )
load( workspaceDataFile_stage_1 )
load( workspaceDataFile_stage_2 )

# ------------------------------------------------------ Main ---------------------------------------------------------
#

positiveCounts = getPositiveCounts(matrixData)
biggestCounts = positiveCounts[order(positiveCounts[,ncol(positiveCounts)],decreasing=TRUE)[seq_len(nrow(positiveCounts))],]

Attributes = as.character(1:ncol(matrixData))

eliminatedColumns = c()	#	Keep all

#
#	Strains matrix (is it different from the one used in "1-strains-preprocess.R" ????)
#	It looks to be the same file, but the one here contains a header... Why??
#
print( paste( "[", format(Sys.time(), "%m/%d/%y %H:%M:%S"),"] Reading Strains File Name...", sep="") )
strainsFileName = input_matrix_2
strains = read.table( strainsFileName, header=TRUE, sep="\t" )

#
#	Define an annotation of the form "S1,S2,..."
#	Column i receives label Si
#

print( paste( "[", format(Sys.time(), "%m/%d/%y %H:%M:%S"),"] Number of Cols:", ncol(strains), sep="") )

print( paste( "[", format(Sys.time(), "%m/%d/%y %H:%M:%S"),"] Creating Short Labels...", sep="") )
shortLabels = sapply(seq_len(ncol(strains)),function(x) { paste('S',x,sep='') })
print( shortLabels )	# Should be number of columns

print( paste( "[", format(Sys.time(), "%m/%d/%y %H:%M:%S"),"] Writing Short to Actual Annotations...", sep="") )
fileWithShortAnnotations = paste( outputDirForGroup, nChainIterations, "-/shortTOactualAnnotation.csv", sep="" )
write.csv(cbind( matrix( shortLabels, ncol=1 ), matrix( colnames( strains ), ncol=1 ) ), file=fileWithShortAnnotations )


retainedColumns = setdiff(1:ncol(strains),eliminatedColumns)
annotationColumns = shortLabels[retainedColumns]


# -------------------------------------------------- Markov Chains --------------------------------------------------
#
#
print( paste( "[", format(Sys.time(), "%m/%d/%y %H:%M:%S"),"] Loading First RData file...", sep="") )

results = list()

#
#	Loads the First output file from the Markov chains.  The index for this RData file should be the first index from
#	seq(from=1,to=10,by=1) in "2-strains-parallel.R"
#
load( paste( outputDirForGroup, "/R_data/", sprintf( "search-%d.RData", 1 ), sep="" ) )

results[[1]] = list(bestModel=bestModel,bestBIC=bestBIC,bestGraph=bestGraph,bestEstimates=bestEstimates,bestIndividual=bestIndividual)

allModels = modelList

print( paste( "[", format(Sys.time(), "%m/%d/%y %H:%M:%S"),"] Loading Remaining RData Pieces (", nChainReplicates, ")", sep="") )

#
#	Load the rest of the output files from the Markov chains.  The 'load()' call above loads the first output file, and this
#	loop loads the remaining ones...
#
for(i in 2:nChainReplicates )
{
	
	print( paste( "[", format(Sys.time(), "%m/%d/%y %H:%M:%S"),"] Loading ", sprintf("search-%d.RData",i), sep="") )
	
	# load(sprintf("search-%d.RData",i))
	load( paste( outputDirForGroup, "/R_data/", sprintf("search-%d.RData",i), sep="" ) )
  
	results[[i]] = list(bestModel=bestModel,bestBIC=bestBIC,bestGraph=bestGraph,bestEstimates=bestEstimates,bestIndividual=bestIndividual)
	allModels = concatenateModelList(allModels,modelList)
  
}

#
#	BMA Graph
#

print( paste( "[", format(Sys.time(), "%m/%d/%y %H:%M:%S"),"] Creating BMA Graph...", sep="") )
bmaGraph = as.matrix(averageGraph(allModels))


print( paste( "[", format(Sys.time(), "%m/%d/%y %H:%M:%S"),"] BMA Graph — rows: ", nrow(bmaGraph),", cols: ", ncol(bmaGraph), sep="") )
bmaEstimates = averageEstimates(allModels)

medianGraph = bmaGraph

for(i in 1:nrow(medianGraph))
{
  for(j in 1:ncol(medianGraph))
  {
    medianGraph[i,j] = ifelse(bmaGraph[i,j]>=0.5,1,0)
  }
}


# -------------------------------------------------- Find the Best Model ----------------------------------------------
#
#

print( paste( "[", format(Sys.time(), "%m/%d/%y %H:%M:%S"),"] Top Cell Estimates ...", sep="") )

bestmodel = which.max(sapply(results,function(x) x$bestBIC))

y = cbind(biggestCounts,results[[bestmodel]]$bestEstimates,bmaEstimates)

topCellEstimatesFile = paste( outputDirForGroup, "/", nChainIterations, "-topcellestimates.txt", sep="" )
write( t(y), file=topCellEstimatesFile, ncol=ncol(y) )

#
#	Best Graph PDF
#
print( paste( "[", format(Sys.time(), "%m/%d/%y %H:%M:%S"),"] Best Graph...", sep="") )
bestGraphsFilePDF = paste( outputDirForGroup, "/", nChainIterations, "-bestgraph.pdf", sep="" )

CairoPDF( file=bestGraphsFilePDF )
x=which(apply(results[[bestmodel]]$bestGraph,2,sum)>1)
gplot(as.matrix(results[[bestmodel]]$bestGraph[x,x]),gmode="graph",label=Attributes[x])
dev.off()


#
#	Median Graph PDF
#
print( paste( "[", format(Sys.time(), "%m/%d/%y %H:%M:%S"),"] Median Graph...", sep="") )
medianGraphsFilePDF = paste( outputDirForGroup, "/", nChainIterations, "-mediangraph.pdf", sep="" )

CairoPDF( file=medianGraphsFilePDF )
x=which(apply(medianGraph,2,sum)>1)
gplot(as.matrix(medianGraph[x,x]),gmode="graph",label=Attributes[x])
dev.off()


#
#	Convergence plot
#
print( paste( "[", format(Sys.time(), "%m/%d/%y %H:%M:%S"),"] Convergence Plot...", sep="") )
convergenceFile = paste( outputDirForGroup, "/", nChainIterations, "-convergence.pdf", sep="" )

CairoPDF( file=convergenceFile, 14, 10 )
y = sapply(results,function(x) x$bestBIC)
plot(1:length(results),sort(y)-min(y),type="p",xlab="Search replicate",ylab="(Shifted) Best BIC found")
dev.off()


#
#	Heatmap
#
print( paste( "[", format(Sys.time(), "%m/%d/%y %H:%M:%S"),"] BMA Graph Heatmap...", sep="") )
bmaGraphHeatmapFilePDF = paste( outputDirForGroup, "/", nChainIterations, "-bmagraph-heatmap.pdf", sep="" )

rownames(bmaGraph) = annotationColumns
colnames(bmaGraph) = annotationColumns

CairoPDF( file=bmaGraphHeatmapFilePDF, 17, 17 )
pheatmap(bmaGraph,show_rownames=TRUE,show_columnnames=TRUE)
dev.off()


#
#	Best Graph Dot Plot
#

print( paste( "[", format(Sys.time(), "%m/%d/%y %H:%M:%S"),"] Best Graph Dot Plot...", sep="") )
bestGraphDotFile = paste( outputDirForGroup, "/", nChainIterations, "-bestGraph.dot", sep="" )
graphToDot( as.matrix( results[[bestmodel]]$bestGraph ), annotationColumns, bestGraphDotFile, TRUE, FALSE )


#
#	BMA Graph Dot Plot
#
print( paste( "[", format(Sys.time(), "%m/%d/%y %H:%M:%S"),"] BMA Graph Dot Plot...", sep="") )
bmaGraphDotFile = paste( outputDirForGroup, "/", nChainIterations, "-bmaGraph.dot", sep="" )
graphToDot( bmaGraph,annotationColumns, bmaGraphDotFile, TRUE, FALSE )


# --------------------------------------------- Old Final Results ---------------------------------------------
#
#
print( paste( "[", format(Sys.time(), "%m/%d/%y %H:%M:%S"),"] Old Final Results...", sep="") )
y=read.table( topCellEstimatesFile, header=FALSE )

myresults = numeric(ncol(y)-3)

for(i in 1:(ncol(y)-3)) {
	a=by(y[,ncol(y)],y[,i],sum)
	myresults[i] = a[2]/sum(a)
}  

round(myresults*1000)/1000


# --------------------------------------------- New Final Results ---------------------------------------------
#
#

print( paste( "[", format(Sys.time(), "%m/%d/%y %H:%M:%S"),"] New Final Results...", sep="") )
allData = read.table(file=input_matrix_1,header=FALSE)

#	THIS ASSUMES THE DATA ARE CODED AS 0 AND 1
columnSumsAllData = apply(allData,2,sum)

#	TThe last column is (1,1,...,1)
bmaIndividual = averageIndividual(allModels)
columnSumsAllData[retainedColumns] = bmaIndividual[1:length(retainedColumns)]
columnSumsAllData[1+length(columnSumsAllData)] = bmaIndividual[length(bmaIndividual)]

names(columnSumsAllData) = c(apply(cbind(matrix(shortLabels,ncol=1),matrix(colnames(strains),ncol=1)),1,
                                   function(x) { paste(x[1],' - ',x[2],sep='')}),"Unknown")

x=sort(columnSumsAllData/sum(columnSumsAllData),decreasing=TRUE)


#
#	Final Probabilities (1st) File
#
print( paste( "[", format(Sys.time(), "%m/%d/%y %H:%M:%S"),"] Final Probabilities, 1st...", sep="") )
finalProbsFirstFile = paste( outputDirForGroup, "/", nChainIterations, "-finalprobs-first.pdf", sep="" )

pdf( file=finalProbsFirstFile, 10, 6 )

par(las=2)	#	Make label text perpendicular to axis
par(mar=c(5.1, 20.1, 4.1, 2.1))	#	Increase y-axis margin.
barplot(sort(x[1:5]),horiz=TRUE)
dev.off()


#
#	Final Probabilities (2nd) File
#
print( paste( "[", format(Sys.time(), "%m/%d/%y %H:%M:%S"),"] Final Probabilities, 2nd...", sep="") )
finalProbsSecondFile = paste( outputDirForGroup, "/", nChainIterations, "-finalprobs-second.pdf", sep="" )

pdf( file=finalProbsSecondFile, 10, 6 )

par(las=2)	#	Make label text perpendicular to axis
par(mar=c(5.1, 20.1, 4.1, 2.1))	#	Increase y-axis margin.
barplot(sort(x[6:30]),horiz=TRUE)
dev.off()


# --------------------------------------- Plot the number of neighbors Graph ---------------------------------------
#
#

numberNeighbors = numeric(ncol(strains))
numberNeighbors[retainedColumns] = apply(gMax,2,sum)/length(numberNeighbors)
names(numberNeighbors) = apply(cbind(matrix(shortLabels,ncol=1),matrix(colnames(strains),ncol=1)),1,
                               function(x) { paste(x[1],' - ',x[2],sep='')})


print( paste( "[", format(Sys.time(), "%m/%d/%y %H:%M:%S"),"] Plotting Number of Neighbors...", sep="") )

#
#	File for the number of neighbors
#
barPlotForGMax = paste( outputDirForGroup, "/", nChainIterations, "-gMax-bars.pdf", sep="" )

pdf( file=barPlotForGMax, 10, 24 )

par(las=2) # make label text perpendicular to axis
par(mar=c(5.1, 22.1, 4.1, 2.1)) # increase y-axis margin.
barplot(sort(numberNeighbors,decreasing=FALSE),horiz=TRUE)
dev.off()
	

# -------------------------------------------------- END Stage 3 ------------------------------------------------------

print( paste( "[", format(Sys.time(), "%m/%d/%y %H:%M:%S"),"] ", sep="") )
print( paste( "[", format(Sys.time(), "%m/%d/%y %H:%M:%S"),"] Stage 3, Done.", sep="") )
print( paste( "[", format(Sys.time(), "%m/%d/%y %H:%M:%S"),"] ", sep="") )

