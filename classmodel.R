library(stringr)

setClass("modelsList",
         representation(nModels = "numeric",
                        maxBIC="numeric",
                        minBIC="numeric",
                        bics="list",
                        graphs="list",
                        estimates="list",
                        individual="list"))

epsilonModels = 1e-4

createModelList <- function(myBIC,myGraph,myEstimates,myIndividual)
{
  x = new("modelsList")
  x@nModels = 1
  x@maxBIC = myBIC
  x@minBIC = myBIC
  x@nModels = 1
  x@bics = list()
  x@graphs = list()
  x@estimates = list()
  x@individual = list()

  x@bics[[1]] = myBIC
  x@graphs[[1]] = myGraph
  x@estimates[[1]] = myEstimates
  x@individual[[1]] = myIndividual
  
  return(x)
}

isModelInList <- function(x,myBIC)
{
  return(is.element(myBIC,unlist(x@bics)))
}

insertModelInList <- function(x,myBIC,myGraph,myEstimates,myIndividual)
{
  if((myBIC<x@minBIC+log(epsilonModels))|isModelInList(x,myBIC))
  {
    return(x)
  }
    
  #insert the model in the list
  x@nModels = x@nModels+1
  x@bics[[x@nModels]] = myBIC
  x@graphs[[x@nModels]] = myGraph
  x@estimates[[x@nModels]] = myEstimates
  x@individual[[x@nModels]] = myIndividual
  
  #we found a better model
  if(myBIC>x@maxBIC)
  {
    x@maxBIC = myBIC
    
    #we need to eliminate models that are too bad
    #with respect to the new best model
    mymodels = which(sapply(x@bics,function(y) exp(y-x@maxBIC)<epsilonModels))
    
    x@bics[mymodels] = NULL
    x@graphs[mymodels] = NULL
    x@estimates[mymodels] = NULL
    x@individual[mymodels] = NULL
    
    x@minBIC = min(unlist(x@bics))
    x@nModels = length(x@bics)
  }
  
  return(x)
}

concatenateModelList <- function(x1,x2)
{
  for(i in seq_len(x2@nModels))
  {
    x1 = insertModelInList(x1,x2@bics[[i]],x2@graphs[[i]],x2@estimates[[i]],x2@individual[[i]])
  }
  return(x1)
}  

scaleBICs <- function(x)
{
  y = exp(unlist(x@bics)-x@maxBIC)
  return(y/sum(y))
}

averageGraph <- function(x)
{
  w = scaleBICs(x)
  mygraph = w[1]*x@graphs[[1]]
  if(x@nModels>=2)
  {
    for(i in 2:x@nModels)
    {
      mygraph = mygraph+w[i]*x@graphs[[i]]
    }
  }
  return(mygraph)
}

averageEstimates <- function(x)
{
  w = scaleBICs(x)
  myestimates = w[1]*x@estimates[[1]]
  if(x@nModels>=2)
  {
    for(i in 2:x@nModels)
    {
      myestimates = myestimates+w[i]*x@estimates[[i]]
    }
  }
  return(myestimates)
}

averageIndividual <- function(x)
{
  w = scaleBICs(x)
  myestimates = w[1]*x@individual[[1]]
  if(x@nModels>=2)
  {
    for(i in 2:x@nModels)
    {
      myestimates = myestimates+w[i]*x@individual[[i]]
    }
  }
  return(myestimates)
}
