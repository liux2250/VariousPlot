#The scripture in this file is used to draw stacked, group and percent bar plot
#https://r-graph-gallery.com/48-grouped-barplot-with-ggplot2
#https://statisticsglobe.com/calculate-percentage-group-r    (calculate percentage)
library("ggplot2")
library("scales")
library("dplyr")
# create a dataset
specie <- c(rep("sorgho" , 3) , rep("poacee" , 3) , rep("banana" , 3) , rep("triticum" , 3) )
condition <- rep(c("normal" , "stress" , "Nitrogen") , 4)
value <- abs(rnorm(12 , 0 , 15))
data <- data.frame(specie,condition,value)
data$value <- round(data$value,2)

#######Non-grouping bar plot#####
ggplot(data, aes(fill=condition, y=value, x=rownames(data))) + 
    geom_bar(position="dodge",stat="identity",width = 0.8)+ #group barplot
    scale_y_continuous(expand = expansion(mult = c(0, .1)))+
    geom_text(aes(label = value), vjust = 0.6, colour = "black",size=6, position = position_dodge(width = 0.8))
#######Non-grouping bar plot#####

######### Grouped bar plot #########
ggplot(data, aes(fill=condition, y=value, x=specie)) + #when multiple samples with the same x value (specie in this case), grouping is automatically performed
  geom_bar(position="dodge",stat="identity",width = 0.8)+ #group barplot
  scale_y_continuous(expand = expansion(mult = c(0, .1)))+
  geom_text(aes(label = value), vjust = 0.6, colour = "black",size=6, position = position_dodge(width = 0.8))
######### Grouped bar plot #########
#Stacked bar plot
ggplot(data, aes(fill=condition, y=value, x=specie)) + 
  geom_bar(position="stack", stat="identity")+ ##default is stacked barplot
  scale_y_continuous(expand = expansion(mult = c(0, .1)))+
  geom_text(aes(label = value), colour = "black",size=6,position = position_stack(vjust = .5))

#Stacked percentage bar plot

Perdata <- data %>%                                    # Calculate percentage by group
  group_by(specie) %>%                                 #group_by and the following mutate functions are from "dyplr" package
  mutate(pct = value / sum(value)) %>% 
  as.data.frame()
Perdata$pct <- round(Perdata$pct,2)


ggplot(Perdata, aes(fill=condition, y=pct, x=specie)) + 
  geom_bar(position="fill", stat="identity")+ ##default is stacked barplot
  scale_y_continuous(expand = expansion(mult = c(0, .1)),labels = scales::percent)+
  geom_text(aes(label=scales::percent(pct)), position = position_stack(vjust = .5))


