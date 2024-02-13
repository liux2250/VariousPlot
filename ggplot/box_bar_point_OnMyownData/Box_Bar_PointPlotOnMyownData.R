library("ggplot2")
library("ggfortify")



RawCountOriginal <- read.delim("cohort2_55Quqlified_rawcount.tsv",sep="\t",header=TRUE,as.is=TRUE,check.names = FALSE)
coverSumOriginal <- read.delim("Cohort2_102Sample_Coverage_Summary.txt",sep="\t",header=TRUE,as.is=TRUE,check.names=FALSE)

sampleName <- names(RawCountOriginal)[3:ncol(RawCountOriginal)]
index <- match(sampleName,coverSumOriginal[,25]) 

CoverageInfoOriginal <- coverSumOriginal[index,c(15,11,12,38,16,40)]#"RNA Filtered Duplicate Rate","RNA Detected Genes","RNA Exonic Rate","RNA Input Reads","RNA Mean Per Base Coverage"
colnames(CoverageInfoOriginal) <- c("DuplicateRate","NumGenes","ExonicRate","InputReads","MappedPct", "MeanPerBaseCov")
rownames(CoverageInfoOriginal) <- sampleName


##########Read the data for original 45 re-sequenced samples########
RawCountReseq <- read.delim("cohort2_45Resequence_rawcount.tsv",sep="\t",header=TRUE,as.is=TRUE,check.names = FALSE)
coverSumReseq <- read.delim("Cohort2_45Resequence_Coverage_Summary.txt",sep="\t",header=TRUE,as.is=TRUE,check.names=FALSE)

sampleName <- names(RawCountReseq)[3:ncol(RawCountReseq)]
index <- match(sampleName,coverSumReseq[,25]) 

CoverageInfoReseq <- coverSumReseq[index,c(15,11,12,44,16,46)]
colnames(CoverageInfoReseq) <- c("DuplicateRate","NumGenes","ExonicRate","InputReads","MappedPct", "MeanPerBaseCov")
rownames(CoverageInfoReseq) <- sampleName


##########Read the data for original 45 re-sequenced samples########

##############DrawBoxplot###############################
CoverageInfo <- rbind(CoverageInfoOriginal,CoverageInfoReseq)#These two scripts are specially for drawing multiple Boxplots into one figure 
CoverageInfo  <- rbind(CoverageInfo,CoverageInfo) #Specially for box plot with multiple boxes



NumOriginal <- nrow(CoverageInfoOriginal)
NumReseq <- nrow(CoverageInfoReseq)

CoverageInfo$Batch <- "original"
CoverageInfo$Batch[(NumOriginal+1):(NumOriginal+NumReseq)] <- "reseq"
CoverageInfo$Batch[(NumOriginal+NumReseq+1):(2*(NumOriginal+NumReseq))] <- "merged"

##############DrawBoxplot###############################
for (loop in 1:(ncol(CoverageInfo)-1))
{
  outputName <- paste(names(CoverageInfo)[loop],"Boxplot",".pdf",sep="_")
  pdf(outputName,width=30, height=20)
  print(ggplot(CoverageInfo, aes(x=CoverageInfo[,7], y=CoverageInfo[,loop])) + 
    geom_boxplot(outlier.colour="red", outlier.shape=8,outlier.size=8)+
    theme(text = element_text(size=40))+xlab("samples") + ylab(names(CoverageInfo)[loop]))
  dev.off() #For the ggplot function, if we want to output the figure to a pdf file, we have to add "print"
}
##############DrawBoxplot###############################

##############Draw vertical Barplot###############################
CoverageInfo <- rbind(CoverageInfoOriginal,CoverageInfoReseq)


for (loop in 1:ncol(CoverageInfo))
{
  outputName <- paste(names(CoverageInfo)[loop],"Barplot",".pdf",sep="_")
  pdf(outputName,width=30, height=20)
  p <- ggplot(CoverageInfo, aes(x=reorder(rownames(CoverageInfo),+CoverageInfo[,loop]), y=CoverageInfo[,loop],fill=rownames(CoverageInfo))) + 
    geom_bar(stat="identity",orientation="x")+
    theme(axis.text.x=element_text(angle=90,size=20),legend.position="none",
          axis.title=element_text(size=50),axis.text.y=element_text(size=20))+
    xlab("samples") + ylab(names(CoverageInfo)[loop])+scale_y_continuous(expand = expansion(mult = c(0, .1)))+
    geom_text(aes(label = CoverageInfo[,loop]), angle=90,vjust = 0.6, colour = "black",size=6)
  print(p)
  dev.off()
} 

#The above script is only used to draw the bar plot of a vector 

##### Generate horizontal bar plot#####
for (loop in 1:ncol(CoverageInfo))
{
  outputName <- paste(names(CoverageInfo)[loop],"Barplot",".pdf",sep="_")
  pdf(outputName,width=30, height=20)
  p <- ggplot(CoverageInfo, aes(x=reorder(rownames(CoverageInfo),+CoverageInfo[,loop]), y=CoverageInfo[,loop],fill=rownames(CoverageInfo))) +
    geom_bar(stat="identity",orientation="x")+
    theme(axis.text=element_text(size=16),legend.position="none",
    axis.title=element_text(size=50))+scale_y_continuous(expand = expansion(mult = c(0, .1)))+
    geom_text(aes(label = CoverageInfo[,loop]), vjust = 0.6, colour = "black",size=6)+
    xlab("samples") + ylab(names(CoverageInfo)[loop])+coord_flip()
  print(p)
  dev.off()
} #The difference between vertical and horizontal bar plot, lies in the them and add "coord_flip()"


############draw point plot ###############3
CoverageInfo <- rbind(CoverageInfoOriginal,CoverageInfoReseq)
NumOriginal <- nrow(CoverageInfoOriginal)
NumReseq <- nrow(CoverageInfoReseq)

CoverageInfo$Batch <- "original"
CoverageInfo$Batch[(NumOriginal+1):(NumOriginal+NumReseq)] <- "reseq"

Thresholds=c(50,10000,0.6,20,80,20)


for (loop in 1:(ncol(CoverageInfo)-1))
{
  outputName <- paste(names(CoverageInfo)[loop],"Pointplot",".pdf",sep="_")
  pdf(outputName,width=30, height=20)
  p <- ggplot(CoverageInfo, aes(x=reorder(rownames(CoverageInfo),+CoverageInfo[,loop]), y=CoverageInfo[,loop],fill=rownames(CoverageInfo))) + 
    # geom_point(aes(colour = factor(Batch)),size=6)+
    geom_point(size=6)+
    theme(axis.text.x=element_text(angle=90,size=20),legend.position="none",
          axis.title=element_text(size=50),axis.text.y=element_text(size=20))+
    xlab("samples") + ylab(names(CoverageInfo)[loop])+
    geom_hline(yintercept=Thresholds[loop], linetype="dashed", color = "red",linewidth=2)
  print(p)
  dev.off()
} 