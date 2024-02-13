#https://rpkgs.datanovia.com/ggpubr/reference/ggdotplot.html
#http://www.sthda.com/english/articles/24-ggpubr-publication-ready-plots/76-add-p-values-and-significance-levels-to-ggplots/

#For two-group comparison, the default method of the "stat_compare_means" function in the "ggpubr" package is wilcox.test
#For multiple-group comparison, the default method of the "stat_compare_means" function in the "ggpubr" package is Kruskal−Wallis
#Namely, the default statistic comparison method is non-parametric approach

library("ggpubr")
library("ggplot2")


MCPCounterResult <- read.delim("MCPCounterResult.tsv",sep="\t",header=TRUE,as.is=TRUE,check.names = FALSE)
ClinicalInfo <- read.delim("ClinicalInfo.txt",sep="\t",header=TRUE,as.is=TRUE,check.names = FALSE)
# ClinicalInfo <- ClinicalInfo[-15,]
# MCPCounterResult <- MCPCounterResult[,-15]
#This two sentences are only suitable for ColourFeature <- ClinicalInfo$ECMIndicator
#

NumSample <- nrow(ClinicalInfo)
NumMCPFeature <- nrow(MCPCounterResult)

ColourFeature <- ClinicalInfo$IOIndicator ### This should be changed manually

Target <- data.frame(value = c(t(MCPCounterResult)))
Target$MCPFeature <- rep(rownames(MCPCounterResult),each=NumSample)
Target$ColourFeature <- rep(ColourFeature,time=NumMCPFeature)
# Target$ColourFeature = factor(Target$ColourFeature,levels=c("XRT","No XRT"))
#Only needed when ColourFeature = ClinicalInfo$XRTIndicator

pdf("IO.pdf",width=8, height=6)  

p <- ggboxplot(Target, x = "MCPFeature", y = "value",ylab ="MCPCounter Score",
               color = "ColourFeature", palette="lancet",
               bxp.errorbar=TRUE,x.text.angle=45,xlab=FALSE)
p <- p+stat_compare_means(aes(group = ColourFeature),
                          label = "p.signif",
                          label.y=16)
                         

p <- p+rremove("legend.title")
print(p)
dev.off()

#########This part is used to calculate the hedges'g##
library("effectsize")
data(mtcars)
mtcars$am <- factor(mtcars$am)
hedges_g(mpg ~ am, data = mtcars,pooled_sd = FALSE)


