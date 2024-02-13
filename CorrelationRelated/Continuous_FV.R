library("coin")


##############(1)  Read and prepare data########
RawCount <- read.delim("RawCount_RemoveOutlier_ERA0.8.tsv",sep="\t",header=TRUE,as.is=TRUE,check.names = FALSE)
coverSum <- read.delim("Cohort1_coverage_summary.txt",sep="\t",header=TRUE,as.is=TRUE,check.names=FALSE)
DataSum <- read.delim("Datasummary_v5.txt",sep="\t",header=TRUE,as.is=TRUE,check.names=FALSE)
LNSum <- read.delim("cohort1_ALL_LN.txt",sep="\t",header=TRUE,as.is=TRUE,check.names=FALSE)

#The "Cohort1_coverage_summary.txt","Datasummary_v5.txt" and "cohort1_ALL_LN.txt"
#match each other by MRN

#############Generate feature matrix#########
RawCountMatrix <- RawCount[,3:ncol(RawCount)]
row.names(RawCountMatrix) <- RawCount[,1]

LNSum$DISSECTION_GREATEST_DIAM_CHAR <- as.numeric(LNSum$DISSECTION_GREATEST_DIAM_CHAR)
#If we did not convert this column as numeric, it returns wrong result when we extract this diameter in the following
#section.
sampleName <- names(RawCount)[3:ncol(RawCount)]
index <- match(sampleName,coverSum[,25])
MRN <- coverSum[index,2]
DISSECTION_GREATEST_DIAM_CHAR <- c()

num <- 1
for (loop in 1:length(MRN))
{
  
  index <- which(LNSum$MRN %in% MRN[loop] & 
                   LNSum$PROCEDURE_TYPE == "Sentinel Lymph Node Biopsy" &
                   LNSum$STAGING_POSITION == "Yes") 
  DISSECTION_GREATEST_DIAM_CHAR[num] <- max(LNSum[index,"DISSECTION_GREATEST_DIAM_CHAR"])
  num <- num +1
}


index <- match(MRN,DataSum$MRN)
ExtrColDataSum <- c("Outcome_Grouping","gender","Breslow_Thickness_MM","Ulceration","Mitoses","Lymphovascular_Invasion","Lymphocytic_Infiltrate",
                    "Microsatellites","Tcat","AGE_FIRST_PRIM","LAST_VITALSTATUS","CAUSE_OFDEATH","Primary_DX_Year",
                    "Primary_Site_Anatomic_Category")
#In this version of script,
#"Breslow_Thickness_MM", "Mitoses","AGE_FIRST_PRIM","Primary_DX_Year" and DGDC are all continuous factors

#Always put the interested event ("Outcome_Grouping" in this project), as the 1st colum of Features.
#Otherwise, an error will occur in the downstream analysis.


ClinicalInfo <-  DataSum[index,ExtrColDataSum]


ClinicalInfo$DGDC <- DISSECTION_GREATEST_DIAM_CHAR
ClinicalInfo$Breslow_Thickness_MM <- as.numeric(ClinicalInfo$Breslow_Thickness_MM)
ClinicalInfo$Mitoses <- as.numeric(ClinicalInfo$Mitoses)
##############(1)  Read and prepare data########

##############(2) Define interested features

drop <- c("CAUSE_OFDEATH")  #For this feature, so many unknow, so it will be not considered as a feature
Features <- ClinicalInfo[!names(ClinicalInfo) %in% drop]


##############(2) Define interested features

#############(3)Delete the samples with "unknown" in the features#### 

DetectUnknown <- function(x)
{
  temp <- grep("Unknown",x)
  if(length(temp)>0)
  {
    return("TRUE")
  }else{
    return("FALSE")
  }
}
toDrop <- apply(Features,1,DetectUnknown) 

index <- grep("TRUE",toDrop)
if(length(index)>0)
{
  RawCountMatrix <- RawCountMatrix[,-index]
  Features <- Features[-index,]
  
}

write.table(Features, file = "ContinuousFeatures.tsv", sep = "\t", col.names = TRUE,
            row.names = FALSE, quote = FALSE,na="")
############Nominal/Nominal association###########
NomFeatures <- Features[,c(1,2,4,6:8,11,13)] #Extract 8 nominal features
#1) gener: Male and Femal two levels
#2) Ulceration: "Absent" and "Present" two levels
#3) Lymphovascular_Invasion:"Absent" and "Present" two levels
#4) Lymphocytic_Infiltrate: "Non-Brisk","Minimal" and "Absent" three levels
#5) Microsatellites: "Absent" and "Present" two levels
#6) LAST_VITALSTATUS:"Alive" and "Dead" two levels
#7)Primary_Site_Anatomic_Category: "UpperExtremity", "LowerExtremity", "Trunk" and "Head_OR_Neck" 4 levels


NomNomTest <- function(Features)
{
  out <-  as.data.frame(matrix(nrow=ncol(Features)-1,ncol=3))
  rownames(out) <- colnames(Features)[2:ncol(Features)]
  colnames(out) <- c("X-squared","df","p-value")
  extract.fun = function(x)c(x[[1]],x[[2]],x[[3]])  
  for (loop in 2: ncol(Features))
  {
    result <- chisq.test(table(Features[,1],Features[,loop]))
    if (min(result$expected) > 5)
    {  #If the expected frequency is below 5, which means the Chi-square result might be not reliable
      #and fisher test is needed
      out[loop-1,] <- extract.fun(result)
    }else{ #For fisher test, no x-squre and df and only the p-value will be returned.
      result <- fisher.test(table(Features[,1],Features[,loop]))
      out[loop-1,"p-value"] <- result$p.value
    }
    
  }
  
  return(out)              
}

Result <- NomNomTest(NomFeatures)

Result = Result[!is.na(Result[,3]),]  #Delete the comparison result with p-value equals NA
Result$padj = p.adjust(Result[,"p-value"],method="holm")
Result

############Nominal/Nominal association###########


############Nominal/ordinal association###########
OrdFeatures <- Features[,c(1,9)] #Tcat is a ordinal variable

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
    #The chisq_test with scores is specially for ordinal data (call Cochran-Armitage test)
  }
  
  return(out)              
}

Result <- NomOrdinalTest(OrdFeatures)

Result = Result[!is.na(Result[,1]),]  #Delete the comparison result with p-value equals NA
Result$padj = p.adjust(Result[,"p-value"],method="holm")
Result
############Nominal/ordinal association###########

###########Nominal/Continous association###########
ContiFeatures <- Features[,c(1,3,5,10,12,14)]
shapiro.test(ContiFeatures[,6]) #With a p-value greater than 0.05 indicating a normal distribution
hist(ContiFeatures[,6], col='steelblue')

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

Result <- NomContinuousTest(ContiFeatures)

Result = Result[!is.na(Result[,1]),]  #Delete the comparison result with p-value equals NA
Result$padj = p.adjust(Result[,"p-value"],method="holm")
Result
boxplot(Breslow_Thickness_MM~Outcome_Grouping,data=ContiFeatures,col="light gray")
boxplot(Mitoses~Outcome_Grouping,data=ContiFeatures,col="light gray")
boxplot(AGE_FIRST_PRIM~Outcome_Grouping,data=ContiFeatures,col="light gray")
boxplot(Primary_DX_Year~Outcome_Grouping,data=ContiFeatures,col="light gray")
boxplot(DGDC~Outcome_Grouping,data=ContiFeatures,col="light gray")

