#This script is used to compare the CNV burden of IM-L vs IM-H

library("ggpubr")
library("ggplot2")

testData <- read.delim("testData.tsv",sep="\t",header=TRUE,as.is=TRUE,check.names = FALSE)
testData$IM_Clusters <- factor(testData$IM_Clusters,levels=c("IM-L","IM-H"))

X="IM_Clusters"
Y="CNV"

OutputName <- paste(X,Y,sep="_",".pdf")
pdf(OutputName,width=5, height=5)
p <- ggboxplot(testData, x = X, y = Y,
               add = "jitter",add.params=list(size=2),
               color="IM_Clusters",palette="lancet",ylab="CNV Burden")

p <- p+stat_compare_means(method = "wilcox.test",
                          label.x = 1.5,
                          label.y = 60,
                          size=4)

print(p)
dev.off()
#######Script for two groups comparison####


######Using ggplot to generate the boxplot and scatter dots for validation purpose####
pdf("ggplotBoxplot.pdf",width=5, height=5)
p <- ggplot(data=testData, 
            aes(x=IM_Clusters, y=CNV, colour=IM_Clusters)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=16,
               outlier.size=2, notch=FALSE)+
  geom_jitter(shape=16, position=position_jitter(0.2))
print(p)
dev.off()