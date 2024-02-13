#https://rpkgs.datanovia.com/ggpubr/reference/ggviolin.html
library("ggpubr")

data("ToothGrowth")
df <- ToothGrowth
ggviolin(df, x = "dose", y = "len")

########Use different filling colors to differentiate groups######
pdf("ViolionPlotFillColorForDifferentGroups.pdf",width=10, height=6)  
p <- ggviolin(df, x = "dose", y = "len",
         add = c("jitter","boxplot","mean"),
         color="black", #Specify the line's color
         fill="dose",
         palette = c("#00AFBB", "#E7B800", "#FC4E07"), #Using the different fill colors to differentiate different groups
         alpha=0.2)+
stat_summary(fun = "mean",  #This part display the mean value as a blue point
             geom = "point",
             color = "blue",
             size= 5)+
stat_summary(fun.data = function(x) data.frame(y=40, label = paste("mean=",mean(x))), geom="text")+
theme(legend.position="none")
print(p)
dev.off()
########Use different filling colors to differentiate groups######

#######Use dot color to differentiate different groups#####
pdf("ViolionPlotDotColorForDifferentGroups.pdf",width=10, height=6)  

ggviolin(df, x = "dose", y = "len",
         add = "jitter", #If we add the "boxplot" here, the boxplot will be the same color as the dots
         color="black", #Specify the line's color
         fill="white",
         add.params = list(color="dose",size=3),
         palette=c("#81D8D0", "#FAEBD7", "#FC4E07"))+
         # palette=c("#00AFBB", "#E7B800", "#FC4E07"))+ 
        #palette="npg")+
  stat_summary(fun = "mean",  #This part display the mean value as a blue point
               geom = "point",
               color = "blue",
               size= 7)+
  stat_summary(fun.data = function(x) data.frame(y=40, label = paste("mean=",mean(x))), geom="text")+
  theme(legend.position="none")
print(p)
dev.off()
##################Use dot colors to differentiate different groups#####
