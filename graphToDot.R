#this function takes a graph specified by its adjancency matrix and creates a DOT file
#the DOT file can be used in GraphViz
#Argumennts:
# mygraph = adjancency matrix of the graph
# mylabels = labels for the vertices of the graph
# OutputFileName = name of the output file to be created
# displayEdgeWeights = TRUE or FALSE; if true, the color of the edge varies according to weight
# the label between vertices i and j equal the corresponding 
# showSingletons = TRUE or FALSE; if true, singletons are shown
#It is best if the output file has extension '.dot'
graphToDot = function(mygraph,mylabels,OutputFileName,displayEdgeWeights,showSingletons)
{
  nVertices = nrow(mygraph)
  #if the graph has one vertex there is nothing to do
  if(1==nVertices) return;
  
  myfile = file(OutputFileName,"w")
  writeLines('graph G {',con=myfile)
  writeLines('  overlap = false;',con=myfile)
  writeLines('  node [shape=ellipse,style=bold,fontsize=18];',con=myfile)
  
  if(showSingletons)
  {
    for(i in seq_len(nVertices))
    {
      writeLines(paste('  ',mylabels[i],' [label=',mylabels[i],'];',sep=''),con=myfile)
    }
  }
  for(i in seq_len(nVertices-1))
  {
    for(j in seq(i+1,nVertices))
    {
      #every non-zero upper diagonal element is an edge
      if(mygraph[i,j]>0)
      {
        if(displayEdgeWeights)
        {
          if(mygraph[i,j]>0.9) {
            writeLines(paste('  ',mylabels[i],' -- ',mylabels[j],' [style=bold,color=red,w=',round(100*mygraph[i,j],digits=0),'];',sep=''),con=myfile)
          } else if(mygraph[i,j]<0.1)
          {
            writeLines(paste('  ',mylabels[i],' -- ',mylabels[j],' [style=bold,color=black,w=',round(100*mygraph[i,j],digits=0),'];',sep=''),con=myfile)
          } else {
            writeLines(paste('  ',mylabels[i],' -- ',mylabels[j],' [style=bold,color=green,w=',round(100*mygraph[i,j],digits=0),'];',sep=''),con=myfile)
          }
        } else {
          writeLines(paste('  ',mylabels[i],' -- ',mylabels[j],';',sep=''),con=myfile)
        }
      }
    }
  }
  writeLines('}',con=myfile)
  
  close(myfile)
  return
}