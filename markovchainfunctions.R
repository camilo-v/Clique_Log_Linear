library(parallel)
library(stringr)
library(Matrix)
library(igraph)


#this function calculates the MLEs associated with a simple chain graph g
#such graphs are decomposable, hence a formula is used in their calculation
#only the MLEs associated with the strictly positive cell counts are calculated
#ARGUMENTS
#   positiveCounts = the output from the function "getPositiveCounts"
#   cliqueMarginals = the marginals associated with the cliques of the graph g
#   matrixData = the full contingency table represented as a data matrix with samples as
#   rows and variables as columns
getMLEs <- function(pC,cliqueMarginals,matrixData)
{
  oneMLE <- function(oneCellIndex)
  {
    x = sum(log(sapply(cliqueMarginals,function(x,y) x[y],oneCellIndex)))
    return(x)
  }
  
  cellIndices = pC[,-ncol(pC),drop=FALSE]
  z = 0
  if(1==nrow(cellIndices))
  {
     z = oneMLE(cellIndices)
  } else
  {
     z = apply(cellIndices,1,oneMLE)
  }
  #need to adjust for the empty separators
  return(exp(z-(length(cliqueMarginals)-1)*log(nrow(matrixData))))
}

findRowIndices <- function(cellsToMonitor,positiveCounts)
{
  mystrings = apply(cellsToMonitor,1,str_c,collapse=",")
  apply(as.matrix(mystrings),1,function(x) which(str_detect(rownames(positiveCounts),fixed(x))))
}

#this function calculates the BIC of a clique graph model
getBIC <- function(positiveCounts,mlesCurrent,nSampleSize,g)
{
  2*sum(positiveCounts[,"y"]*log(mlesCurrent))-log(nSampleSize)*sum(2^sapply(g@cliques,length)-1)
}

recordGraph <- function(g)
{
  x = Matrix(data=0,nrow=g@nVertices,ncol=g@nVertices)
  for(i in seq_len(g@nCliques))
  {
    x[g@cliques[[i]],g@cliques[[i]]] = 1
  }
  return(x)
}

#this function runs a Markov chain on the space of clique graphs g
#The stationary distribution of the chain is exp(-BIC(g))
#ARGUMENTS
#  nChainIterations = number of iterations for the chain
#  matrixData = the full contingency table represented as a data matrix with samples as
#   rows and variables as columns
#  cellsToMonitor = which cells to record their expected values for; the number of rows
#  gives the number of cells
markovChainSimpleChainGraph <- function(processID,positiveCounts,biggestCounts,nChainIterations,matrixData,maxCliqueSize,gMax,outputDirForGroup,source_code_directory)
{ 
	print( paste( "[", format(Sys.time(), "%m/%d/%y %H:%M:%S"),"] Starting Worker Thread ", processID, sep="") )
	
  	#	Output file
  	outputFileName = sprintf("search-%d.RData",processID)
  	outputFileName =paste( outputDirForGroup, "/R_data/", outputFileName, sep="" )
  
	nChainIterationsOld = nChainIterations
  
  if(file.exists(outputFileName))
  {
    load(outputFileName)
  } else
  {
     #number of variables
     nVertices = ncol(matrixData)
     #sample size
     nSampleSize = nrow(matrixData)
  
     individualVariablesCells = matrix(data=1,nrow=nVertices+1,ncol=nVertices+1)
     diag(individualVariablesCells) = 2
     individualVariablesCells[,ncol(individualVariablesCells)] = 0
  
     #generate a random starting graph
     gCurrent = randomCliqueGraph(nVertices,matrixData)  
     #obtain the MLEs of the current graph
     mlesCurrent = getMLEs(biggestCounts,gCurrent@marginals,matrixData)
     individualCurrent = getMLEs(individualVariablesCells,gCurrent@marginals,matrixData)
     bicCurrent = getBIC(biggestCounts,mlesCurrent,nSampleSize,gCurrent)
     graphCurrent = recordGraph(gCurrent)
  
     bestEstimates = mlesCurrent
     bestIndividual = individualCurrent
     bestModel = gCurrent
     bestBIC = bicCurrent
     bestGraph = graphCurrent

    iteration = 0
  }  
  if(nChainIterations<nChainIterationsOld)
  {
    nChainIterations=nChainIterationsOld
  }
  
  while(iteration <= nChainIterations)
  {
    #cat("iteration = ",iteration,"\n")
    iteration = iteration+1
    
    if(iteration%%500==1)
    {
      save(list=ls(),file=outputFileName)
    }
    if(iteration==1)
    {
      modelList = createModelList(bestBIC,bestGraph,bestEstimates,bestIndividual)
      gCurrent = bestModel
      bicCurrent = bestBIC
      graphCurrent = bestGraph
      mlesCurrent = bestEstimates
      individualCurrent = bestIndividual
    }
    
    #generate a candidate graph
    gNext = randomUpdate(gCurrent,matrixData,maxCliqueSize,gMax)
    while(identical(gNext,gCurrent))
    {
      gNext = randomUpdate(gCurrent,matrixData,maxCliqueSize,gMax)
    }
    #obtain the MLEs of the candidate graph
    graphNext = recordGraph(gNext)
    mlesNext = getMLEs(biggestCounts,gNext@marginals,matrixData)
    individualNext = getMLEs(individualVariablesCells,gNext@marginals,matrixData)
    bicNext = getBIC(biggestCounts,mlesNext,nSampleSize,gNext)
    cat("bicNext = ",bicNext,"\n")
  
    modelList = insertModelInList(modelList,bicNext,graphNext,mlesNext,individualNext)
    makemove = ifelse(bicNext>bicCurrent,TRUE,runif(1)<exp(bicNext-bicCurrent))
    if(makemove)
    {
      gCurrent = gNext
      graphCurrent = graphNext
      bicCurrent = bicNext
      mlesCurrent = mlesNext
      individualCurrent = individualNext
      if(bicCurrent>bestBIC)
      {
        bestModel = gCurrent
        bestBIC = bicCurrent
        bestGraph = graphCurrent
        bestEstimates = mlesCurrent
        bestIndividual = individualCurrent
      }
    }
    cat("iteration = ",iteration," bicCurrent = ",bicCurrent," bestBIC = ",bestBIC,"\n")
  } 
  
  save(list=ls(),file=outputFileName)
  
  print( paste( "[", format(Sys.time(), "%m/%d/%y %H:%M:%S"),"] Ending Worker Thread ", processID, sep="") )
  
  return(NULL)
 
}  