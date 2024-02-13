Features <- read.delim("AllCategoricalFeatures.tsv",sep="\t",header=TRUE,as.is=TRUE,check.names = FALSE)


#################chisq_test with multiple levels ordinal or not###########
# OrdFeatures <- Features[,c(1,9)] #One variable with more than two levels
OrdFeatures <- Features[,c(1,2)] #both variables with only two levels

#############Nominal-ordinal test##################
NomOrdinalTest <- function(Features)
{
  out <-  as.data.frame(matrix(nrow=ncol(Features)-1,ncol=3))
  rownames(out) <- colnames(Features)[2:ncol(Features)]
  colnames(out) <- c("X-squared","df","p-value")
  extract.fun = function(x)c(statistic(x)^2,1,pvalue(x)) 

  for (loop in 2: ncol(Features))
  {
    tbw  = table(relapse=Features[,1],pred=Features[,loop])
    out[loop-1,] = extract.fun(chisq_test(tbw, scores = list("pred"=1:nlevels(factor(Features[,loop])))))

  }
  
  return(out)              
}
NomOrdinalTest(OrdFeatures) 
#############Nominal-ordinal test##################

#############Nominal-nominal test##################
NomNomTest <- function(Features)
{
  out <-  as.data.frame(matrix(nrow=ncol(Features)-1,ncol=3))
  rownames(out) <- colnames(Features)[2:ncol(Features)]
  colnames(out) <- c("X-squared","df","p-value")
  extract.fun = function(x)c(statistic(x)^2,1,pvalue(x))
  
  for (loop in 2: ncol(Features))
  {
    tbw  = table(relapse=Features[,1],pred=Features[,loop])
    out[loop-1,] = extract.fun(chisq_test(tbw))
    
  }
  
  return(out)              
}

NomNomTest(OrdFeatures)
#############Nominal-nominal test##################

#############Ordinal-ordinal test##################
OrdinalOrdinalTest <- function(Features)
{
  out <-  as.data.frame(matrix(nrow=ncol(Features)-1,ncol=3))
  rownames(out) <- colnames(Features)[2:ncol(Features)]
  colnames(out) <- c("X-squared","df","p-value")
  extract.fun = function(x)c(statistic(x)^2,1,pvalue(x)) 
  
  for (loop in 2: ncol(Features))
  {
    tbw  = table(relapse=Features[,1],pred=Features[,loop])
    out[loop-1,] = extract.fun(chisq_test(tbw, scores = list("pred"=1:nlevels(factor(Features[,loop])),
                                                             "relapse"=1:nlevels(factor(Features[,1])))))
    
  }
  
  return(out)              
}
OrdinalOrdinalTest(OrdFeatures) 
#############Ordinal-ordinal test##################
