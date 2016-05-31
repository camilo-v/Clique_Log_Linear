library(stringr)

#definition of the clique graph class
#nVertices is the number of vertices in the graph
#cliques represent lists of cliques
#marginals represent the marginal tables associated with each clique
setClass("cliqueGraph",
         representation(nVertices="numeric",
                        cliques="list",nCliques="numeric",
                        marginals="list"))

#this function generates a graph with no edges
randomCliqueGraph <- function(nVertices,matrixData)
{
  x = new("cliqueGraph")
  x@nVertices = nVertices
  x@nCliques = nVertices
  x@marginals = list()
  for(k in seq_len(x@nCliques))
  {
    x@cliques[[k]] = k
    x@marginals[[k]] = createMarginalTable(x@cliques[[k]],matrixData)
  }
  return(x)
}

#Four possible ways of updating a clique graph
#1) Split a clique into two cliques
#2) Join two cliques into a clique
#3) Switch two random vertices from two random cliques
#4) Move a vertex to another clique
#These four possible ways of updating are equally likely to be chosen
randomUpdate <- function(xCopy,matrixData,maxCliqueSize,gMax)
{
  x = xCopy
  possibleMoves = c(1,2,3,4)
  if(x@nVertices==x@nCliques)
  {
    possibleMoves = 2
  }
  if(1==x@nCliques)
  {
    possibleMoves = 1
  }
  mymove = 4
  if(1==length(possibleMoves))
  {
    mymove = possibleMoves
  } else {
    mymove = sample(possibleMoves,1)
  }
    
  if(1==mymove)
  {
    #split a clique
    myclique = sample.int(x@nCliques,1)
    #if we chose a clique with 1 vertex, return
    if(1==length(x@cliques[[myclique]]))
    {
      return(xCopy)
    }
    k = sample.int(length(x@cliques[[myclique]])-1,1)
    y = sample(x@cliques[[myclique]])
    firstClique = sort(y[1:k])
    secondClique = sort(y[(k+1):length(y)])
    
    #delete the clique
    x@cliques[[myclique]] = NULL
    x@marginals[[myclique]] = NULL
    x@nCliques = x@nCliques+1
    
    x@cliques[[x@nCliques-1]] = firstClique
    x@marginals[[x@nCliques-1]] = createMarginalTable(x@cliques[[x@nCliques-1]],matrixData)    
    x@cliques[[x@nCliques]] = secondClique
    x@marginals[[x@nCliques]] = createMarginalTable(x@cliques[[x@nCliques]],matrixData)
  } else if(2==mymove)
  {
    #join two cliques
    mycliques = sample.int(x@nCliques,2)
    newClique = sort(c(x@cliques[[mycliques[1]]],x@cliques[[mycliques[2]]]))
    if(length(newClique)>maxCliqueSize)
    {
      return(xCopy)
    }
    if(any(gMax[newClique,newClique]==0))
    {
      return(xCopy)
    }
    x@cliques[[mycliques[1]]] = newClique
    x@marginals[[mycliques[1]]] = createMarginalTable(newClique,matrixData)
    #if the new clique has marginal counts of zero, we do not move
    if(any(x@marginals[[mycliques[1]]]@table==0))
    {
      return(xCopy)
    }
    x@cliques[[mycliques[2]]] = NULL
    x@marginals[[mycliques[2]]] = NULL
    x@nCliques = x@nCliques-1    
  }
  else if(3==mymove)
  {
    mycliques = sample.int(x@nCliques,2)
    if((1==length(x@cliques[[mycliques[1]]]))&(1==length(x@cliques[[mycliques[2]]])))
    {
      return(xCopy)
    }
    if(1==length(x@cliques[[mycliques[1]]]))
    {
      firstVertex = x@cliques[[mycliques[1]]]
      firstRest = numeric(0)
    } else
    {
      firstVertex = sample(x@cliques[[mycliques[1]]],1)
      firstRest = setdiff(x@cliques[[mycliques[1]]],firstVertex)
    } 
    if(1==length(x@cliques[[mycliques[2]]]))
    {
      secondVertex = x@cliques[[mycliques[2]]]
      secondRest = numeric(0)
    } else
    {
      secondVertex = sample(x@cliques[[mycliques[2]]],1)
      secondRest = setdiff(x@cliques[[mycliques[2]]],secondVertex)
    }
    x@cliques[[mycliques[1]]] = sort(union(secondVertex,firstRest))
    if(any(gMax[x@cliques[[mycliques[1]]],x@cliques[[mycliques[1]]]]==0))
    {
      return(xCopy)
    }
    x@marginals[[mycliques[1]]] = createMarginalTable(x@cliques[[mycliques[1]]],matrixData)
    #if the new clique has marginal counts of zero, we do not move
    if(any(x@marginals[[mycliques[1]]]@table==0))
    {
      return(xCopy)
    }
    x@cliques[[mycliques[2]]] = sort(union(firstVertex,secondRest))
    if(any(gMax[x@cliques[[mycliques[2]]],x@cliques[[mycliques[2]]]]==0))
    {
      return(xCopy)
    }
    x@marginals[[mycliques[2]]] = createMarginalTable(x@cliques[[mycliques[2]]],matrixData)
    #if the new clique has marginal counts of zero, we do not move
    if(any(x@marginals[[mycliques[2]]]@table==0))
    {
      return(xCopy)
    }
  }
  else
  {
    myclique = sample.int(x@nCliques,1)
    if(1==length(x@cliques[[myclique]]))
    {
      myvertex = x@cliques[[myclique]]
    } else
    {
      myvertex = sample(x@cliques[[myclique]],1)
    }  
    if(2==x@nCliques)
    {
      anotherclique = setdiff(1:2,myclique)
    }
    else
    {
      anotherclique = sample(setdiff(1:x@nCliques,myclique),1)
    }
    x@cliques[[anotherclique]] = sort(union(x@cliques[[anotherclique]],myvertex))
    if(length(x@cliques[[anotherclique]])>maxCliqueSize)
    {
      return(xCopy)
    }
    if(any(gMax[x@cliques[[anotherclique]],x@cliques[[anotherclique]]]==0))
    {
      return(xCopy)
    }
    x@marginals[[anotherclique]] = createMarginalTable(x@cliques[[anotherclique]],matrixData)
    #if the new clique has marginal counts of zero, we do not move
    if(any(x@marginals[[anotherclique]]@table==0))
    {
      return(xCopy)
    }
    if(1==length(x@cliques[[myclique]]))
    {
      x@cliques[[myclique]] = NULL
      x@marginals[[myclique]] = NULL
      x@nCliques = x@nCliques-1
    } 
    else
    {
      x@cliques[[myclique]] = sort(setdiff(x@cliques[[myclique]],myvertex))
      x@marginals[[myclique]] = createMarginalTable(x@cliques[[myclique]],matrixData)
    }
  }  
  
  return(x)
}