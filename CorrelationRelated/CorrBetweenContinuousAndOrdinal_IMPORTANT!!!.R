#REF1 (Correlation coefficient and correlation test in R):https://statsandr.com/blog/correlation-coefficient-and-correlation-test-in-r/ 
#REF2 (What Type of Correlation is Appropriate for Nominal and Continuous Data):https://www.crispanalytics.io/post/what-type-of-correlation-is-appropriate-for-nominal-and-continuous-data
library("coin")
library("ggplot2")

Features <- read.delim("ContinuousFeatures.tsv",sep="\t",header=TRUE,as.is=TRUE,check.names = FALSE)
ContiFeatures <- Features[,c(1,3,5,10,12,14)]

######1) Correlation coefficient for continuous-continuous variables. Refer to REF1######
###https://statsandr.com/blog/correlation-coefficient-and-correlation-test-in-r/ 
ggplot(ContiFeatures) +
  aes(x = DGDC, y = Breslow_Thickness_MM) +
  geom_point(colour = "#0c4c8a") +
  theme_minimal()
pairs(ContiFeatures[, c("DGDC", "Mitoses", "Breslow_Thickness_MM")])
#Visualize the data to make sure there is a linear relationship between them
cor.test(ContiFeatures$Breslow_Thickness_MM,ContiFeatures$DGDC)
#A p-value less than 0.05 indicates the correlation between the two variables

######2) Correlation coefficient for one continuous and one ordinal variables ###
CateFeatures <- read.delim("AllCategoricalFeatures.tsv",sep="\t",header=TRUE,as.is=TRUE,check.names = FALSE)

####Data preparation######
testData <- data.frame(ContiFeatures$Mitoses,CateFeatures$Tcat)
colnames(testData) <- c("mitoses","Tcat")

index <- grep("T1a",testData$Tcat)
testData[index,2] <- 1
index <- grep("T1b",testData$Tcat)
testData[index,2] <- 2
index <- grep("T2a",testData$Tcat)
testData[index,2] <- 3
index <- grep("T2b",testData$Tcat)
testData[index,2] <- 4
index <- grep("T3a",testData$Tcat)
testData[index,2] <- 5
index <- grep("T3b",testData$Tcat)
testData[index,2] <- 6
index <- grep("T4a",testData$Tcat)
testData[index,2] <- 7
index <- grep("T4b",testData$Tcat)
testData[index,2] <- 8 #The ordinal data must be decoded as numeric value

testData$Tcat <- as.numeric(testData$Tcat)
cor.test(testData$mitoses,testData$Tcat,method="spearman")
cor.test(testData$mitoses,testData$Tcat,method="kendall")
######2) Correlation coefficient for one continuous and one ordinal variables ###


