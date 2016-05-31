#definition of the contingency tables class
setClass("MarginalTable",
         representation(originalVariables="numeric",table="table"))

#relevant functions

#FUNCTION that creates an object "MarginalTable"
#ARGUMENTS
#   originalVariables = vector of column indices that correspond to the variables to be included in the marginal table
#   matrixData = the full categorical dataset; rows are samples and columns are variables
createMarginalTable <- function(originalVariables,matrixData)
{
  x = new("MarginalTable");
  x@originalVariables = originalVariables
  x@table = table(matrixData[,originalVariables])
  return(x)
}

#FUNCTION that checks whether all the counts of a marginal table are positive
#ARGUMENTS
#   marginal = an object of class "MarginalTable
allCountsPositive <- function(marginal)
{
  all(x@table>0)
}

#FUNCTION that returns the grand total of a table (i.e., the sample size)
#ARGUMENTS
#   marginal = an object of class "MarginalTable
getGrandTotal <- function(marginal)
{
  sum(x@table)
}

#definition of the "[" operator. The idea is to use indices associated
#with the full contingency table to access elements in the marginal table
#ARGUMENTS
#   x = an object of class "MarginalTable
#   i = a vector of indices whose length must be equal with the number of variable in the full table
setMethod("[",
   signature(x = "MarginalTable", i = "ANY", j = "missing"),
   function (x, i) 
   {
      do.call("[",c(quote(x@table),as.list(i[x@originalVariables])))
   }
)


#this function returns a list with all the marginals of a full table
#represented as a data matrix
getMarginals <- function(varsList,fullTable)
{
  lapply(sapply(sapply(varsList,str_split,","),as.integer),createMarginalTable,fullTable)
}

#this function checks whether all the counts of a list of marginal tables
#are strictly positive
checkMarginals <- function(marginalsList)
{
  for(i in seq_len(length(marginalsList)))
  {
    if(!all(marginalsList[[i]]@table>0))
      return(FALSE)
  } 
  return(TRUE)
}

#this function returns the cells that contain strictly positive counts in
#the full table
getPositiveCounts <- function(x)
{
  #if the categories of some categorical variables are
  #coded with zeros, we shift them to be ones
  myindices = which(apply(x,2,min)==0)
  x[,myindices] = x[,myindices]+1
  
  y = table(apply(x,1,str_c,collapse=","))
  z = t(sapply(str_split(names(y),","),as.integer))
  return(cbind(z,y))
}