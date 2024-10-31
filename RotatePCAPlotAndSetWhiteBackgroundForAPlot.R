#In this version of script, there are two points that I addressed:
#(1) Some PCA plots can sometimes appear "flipped" or "upside down" due to the arbitrary nature of the principal components' direction. 
#Two address this issue, I added a line of code:pcaData$PC2 <- -pcaData$PC2 in the GeneratePCA subfunction. 
#If you find the PCA plot is upsidedown or flipped,you can mannually change the direction of PC1 or PC2.

#(2) Sometimes, the background of the figure is gray and we want to set it as white, we can do it by using the two line of code to the theme:
#panel.background = element_rect(fill = "white"),
#axis.line = element_line(color = "black")

library("DESeq2")
library("edgeR")
library("ComplexHeatmap")
library("ggplot2")
library("ggfortify")
library("sva")
library("limma")

SelectedGeneName="1000MostVariableGenes"

CoverageInfo <- read.delim("CoverageInfo_98Samples.tsv",sep="\t",header=TRUE,as.is=TRUE,check.names=FALSE)
RawCount <- read.delim("cohort2_rawcount_98Samples.tsv",sep="\t",header=TRUE,as.is=TRUE,check.names = FALSE)
sampleName <- names(RawCount)[3:ncol(RawCount)]

RawCountMatrix <- RawCount[,3:ncol(RawCount)]
rownames(RawCountMatrix) <- RawCount[,1]
FTR <- 20
NGThre <- 11000
ERAThre <- 0.7

toDrop <- which(CoverageInfo$FilteredTotalReads < FTR | CoverageInfo$NumGenes < NGThre | CoverageInfo$ExonicRate < ERAThre)
#9 samples were removed
RawCountMatrix <- RawCountMatrix[,-toDrop]


CoverageInfo <- CoverageInfo[-toDrop,]


CoverageInfo$FDU <- NA
index <- which(CoverageInfo$FilteredDupRate<0.5)
CoverageInfo[index,"FDU"] <- "FDU<0.5"
index <- which(CoverageInfo$FilteredDupRate>=0.5 & CoverageInfo$FilteredDupRate<0.8)
CoverageInfo[index,"FDU"] <- "0.5<=FDU<0.8"
index <- which(CoverageInfo$FilteredDupRate>=0.8 & CoverageInfo$FilteredDupRate<0.9)
CoverageInfo[index,"FDU"] <- "0.8<=FDU<0.9"
index <- which(CoverageInfo$FilteredDupRate>=0.9 )
CoverageInfo[index,"FDU"] <- "FDU>=0.9"

CoverageInfo$NG <- NA
index <- which(CoverageInfo$NumGenes<16000)
CoverageInfo[index,"NG"] <- "NG<16000"
index <- which(CoverageInfo$NumGenes>=16000 & CoverageInfo$NumGenes<20000)
CoverageInfo[index,"NG"] <- "16000<=NG<20000"
index <- which(CoverageInfo$NumGenes>=20000)
CoverageInfo[index,"NG"] <- "NG>=20000"


CoverageInfo$ERA <- NA
index <- which(CoverageInfo$ExonicRate<0.8)
CoverageInfo[index,"ERA"] <- "ERA<0.8"
index <- which(CoverageInfo$ExonicRate>=0.8 )
CoverageInfo[index,"ERA"] <- "ERA>=0.8"

CoverageInfo$IR <- NA
index <- which(CoverageInfo$InputReads<60)
CoverageInfo[index,"IR"] <- "IR<60"
index <- which(CoverageInfo$InputReads>=60 & CoverageInfo$InputReads<100)
CoverageInfo[index,"IR"] <- "60<=IR<100"
index <- which(CoverageInfo$InputReads>=100)
CoverageInfo[index,"IR"] <- "IR>=100"

CoverageInfo$FTR <- NA
index <- which(CoverageInfo$FilteredTotalReads<60)
CoverageInfo[index,"FTR"] <- "FTR<60"
index <- which(CoverageInfo$FilteredTotalReads>=60 & CoverageInfo$FilteredTotalReads<100)
CoverageInfo[index,"FTR"] <- "60<=FTR<100"
index <- which(CoverageInfo$FilteredTotalReads>=100)
CoverageInfo[index,"FTR"] <- "FTR>=100"

CoverageInfo$MP <- NA
index <- which(CoverageInfo$MappedPct<80)
CoverageInfo[index,"MP"] <- "MP<80"
index <- which(CoverageInfo$MappedPct>=80 & CoverageInfo$MappedPct<90)
CoverageInfo[index,"MP"] <- "80<=MP<90"
index <- which(CoverageInfo$MappedPct>=90)
CoverageInfo[index,"MP"] <- "MP>=90"

CoverageInfo$MPBC <- NA
index <- which(CoverageInfo$MeanPerBaseCov<10)
CoverageInfo[index,"MPBC"] <- "MPBC<10"
index <- which(CoverageInfo$MeanPerBaseCov>=10 & CoverageInfo$MeanPerBaseCov<20)
CoverageInfo[index,"MPBC"] <- "10<=MPBC<20"
index <- which(CoverageInfo$MeanPerBaseCov>=20 & CoverageInfo$MeanPerBaseCov<50)
CoverageInfo[index,"MPBC"] <- "20<=MPBC<50"
index <- which(CoverageInfo$MeanPerBaseCov>=50 )
CoverageInfo[index,"MPBC"] <- "MPBC>=50"


CoverageInfo$Batch = factor(CoverageInfo$Batch)
CoverageInfo$OneGroup = factor(CoverageInfo$OneGroup)
CoverageInfo$FDU = factor(CoverageInfo$FDU)
CoverageInfo$NG = factor(CoverageInfo$NG)
CoverageInfo$ERA = factor(CoverageInfo$ERA)
CoverageInfo$IR = factor(CoverageInfo$IR)
CoverageInfo$FTR = factor(CoverageInfo$FTR)
CoverageInfo$MP = factor(CoverageInfo$MP)
CoverageInfo$MPBC = factor(CoverageInfo$MPBC)



###################################################
dds <- DESeqDataSetFromMatrix(countData=RawCountMatrix,
                              colData=CoverageInfo,
                              design= ~ 1)
vsd <- vst(dds,blind=TRUE)  #Apply Variance stabilizing transformation to the data

NumVarGene <- 1000

Variance <- apply(assay(vsd),1,var)
GeneIndex <- order(Variance,decreasing=TRUE)[1:NumVarGene]

GenerateHeatmap <- function(SelectedGeneName,data,BatchCorrection,meta)
{
  temp <- paste0("BatchCorrection=",BatchCorrection)
  filename <- paste("Heatmap",SelectedGeneName,temp,".pdf",sep="_")
  pdf(filename,width=20, height=10)
  p <- Heatmap(data, 
               column_title = "Samples", 
               row_title = "variable genes",
               row_dend_side = "left",
               row_dend_width = unit(5, "cm"), #
               column_dend_height = unit(1, "cm"),
               
               ##########Heatmap plot with single annotation bar########
               # top_annotation=HeatmapAnnotation(df=feature,annotation_name_side= 'left',
               #                                  simple_anno_size = unit(0.5, "cm")),
               ##########Heatmap plot with single annotation bar########
               
               ##########Heatmap plot with multiple annotation bars########
               top_annotation=HeatmapAnnotation(df=meta[,c(9,11:17)],annotation_name_side= 'left',
                                                simple_anno_size = unit(0.5, "cm"),
                                                annotation_name_gp= gpar(fontsize = 15),
                                                annotation_legend_param = list(title_gp = gpar(fontsize = 15), labels_gp = gpar(fontsize = 15))),
               
               ##########Heatmap plot with multiple annotation bars########
               show_heatmap_legend = FALSE, #disable original legend.
               row_names_gp = gpar(fontsize = 3),
               column_names_gp = gpar(fontsize = 10), # Text size for row names
               # row_title_gp = gpar(fontsize = 20),
               # column_title_gp = gpar(fontsize = 20)
               
  )
  print(p)
  dev.off()
}

GeneratePCA <- function(SelectedGeneName,data,BatchCorrection,Factor="Batch")
{
  temp <- paste0("BatchCorrection=",BatchCorrection)
  filename <- paste("PCA",SelectedGeneName,temp,".pdf",sep="_")
  pdf(filename,width=20, height=10)
  
  pcaData <- DESeq2::plotPCA(data,intgroup=Factor,returnData=TRUE)
  # pcaData$PC2 <- -pcaData$PC2
  percentVar <- round(100*attr(pcaData,"percentVar"))
  p <- ggplot(pcaData,aes(PC1,PC2,color=Batch))+geom_point(size=5)+
    xlab(paste0("PC1:",percentVar[1],"% variance"))+
    ylab(paste0("PC2:",percentVar[2],"% variance"))+
    coord_fixed()+
    theme(legend.position = "right",
          axis.text=element_text(size=30),text = element_text(size=30),
          panel.background = element_rect(fill = "white"),
          axis.line = element_line(color = "black"))
  print(p)
  dev.off()
}


HeatmapAndPCAOfSelectedGenes <- function(SelectedGeneName,GeneIndex,RawCountMatrix,meta,BatchCorrection,batch)
{
  
  SelectedGenesCount <- RawCountMatrix[GeneIndex,] #Perform batch effect correction on 1000 most variable genes
  
  if(BatchCorrection=="TRUE")
  {
    SelectedGenesCount <- sva::ComBat_seq(as.matrix(SelectedGenesCount), batch=batch, group=NULL)
    #ComBat_seq does not change the number and order of genes. 
  }
  
  dds <- DESeqDataSetFromMatrix(countData= SelectedGenesCount,
                                colData=meta,
                                design= ~1)
   vsd <- vst(dds,blind=TRUE) #For large number of selected genes
  # vsd <- varianceStabilizingTransformation(dds, blind = TRUE, fitType = "parametric")
  ScaledCount <- t(scale(t(assay(vsd))))
  GenerateHeatmap(SelectedGeneName,ScaledCount,BatchCorrection,meta)
  GeneratePCA(SelectedGeneName,vsd,BatchCorrection)
}

HeatmapAndPCAOfSelectedGenes(SelectedGeneName,GeneIndex,RawCountMatrix,
                             CoverageInfo,BatchCorrection="TRUE",CoverageInfo$Batch)




