

Features <- read.delim("ContinuousFeatures.tsv",sep="\t",header=TRUE,as.is=TRUE,check.names = FALSE)
ContiFeatures <- Features[,c(1,3,5,10,12,14)]
# ContiFeatures <- Features[,c(13,3,5,10,12,14)] #Report error of "grouping factor must have exactly 2 levels
#############wilcox.test#################
NomContinuousTest <- function(Features)
{
  out <-  as.data.frame(matrix(nrow=ncol(Features)-1,ncol=2))
  rownames(out) <- colnames(Features)[2:ncol(Features)]
  colnames(out) <- c("W","p-value")
  extract.fun = function(x)c(x[[1]],x[[3]])  
  for (loop in 2: ncol(Features))
  {
    
    out[loop-1,] = extract.fun(wilcox.test(as.formula(paste(names(Features)[loop],names(Features)[1],sep = "~")),data=Features))
    #a formula of the form lhs ~ rhs where lhs is a numeric variable giving the data values and rhs a factor with two levels giving the corresponding groups.
    #As we can see that if one continuous variable and one nominal variable (exact two levels) and we want to use the wilcox.test,
    #we can use the formula interface and must put the nominal variable as the second part of formula
  }
  
  return(out)              
}

NomContinuousTest(ContiFeatures)
#############wilcox.test#################
index <- grep("Good",ContiFeatures$Outcome_Grouping)
ContiFeatures[index,1] <- 1
index <- grep("Bad",ContiFeatures$Outcome_Grouping)
ContiFeatures[index,1] <- 0
ContiFeatures$Outcome_Grouping <- as.numeric(ContiFeatures$Outcome_Grouping)
##################cor.test###############
CorTest <- function(Features)
{
  out <-  as.data.frame(matrix(nrow=ncol(Features)-1,ncol=3))
  rownames(out) <- colnames(Features)[2:ncol(Features)]
  colnames(out) <- c("t","df","p-value")
  extract.fun = function(x)c(x[[1]],x[[2]],x[[3]])  
  for (loop in 2: ncol(Features))
  {
    
    out[loop-1,] = extract.fun(cor.test(Features[,loop],Features[,1]))
 }
  
  return(out)              
}

CorTest(ContiFeatures)
##################cor.test###############

##################cor.test with spearman method###############
CorTestSpearman <- function(Features)
{
  out <-  as.data.frame(matrix(nrow=ncol(Features)-1,ncol=2))
  rownames(out) <- colnames(Features)[2:ncol(Features)]
  colnames(out) <- c("S","p-value")
  extract.fun = function(x)c(x[[1]],x[[3]])  
  for (loop in 2: ncol(Features))
  {
    
    out[loop-1,] = extract.fun(cor.test(Features[,loop],Features[,1],method="spearman"))
  }
  
  return(out)              
}

CorTestSpearman(ContiFeatures)

