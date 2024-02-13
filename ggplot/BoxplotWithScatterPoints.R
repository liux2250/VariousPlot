library(tidyverse)
library(hrbrthemes)
library(viridis) #scale_fill_viridis function is in this package

# create a dataset
data <- data.frame(
  name=c( rep("A",500), rep("B",500), rep("B",500), rep("C",20), rep('D', 100)  ),
  value=c( rnorm(500, 10, 5), rnorm(500, 13, 1), rnorm(500, 18, 1), rnorm(20, 25, 4), rnorm(100, 12, 1) )
)


#########Automatically filled the box with different color, but the dots only have one color#####
ggplot(data,aes(x=name, y=value, fill=name))+  #Automatically set the filled colour of box
# ggplot(data,aes(x=name, y=value, color=name)) +  #Automatically set the border colour of box
geom_boxplot(outlier.colour="red", outlier.shape=8,outlier.size=3) +
scale_fill_viridis(discrete = TRUE, alpha=0.6) +
# geom_jitter(color="black", size=0.4, alpha=0.9) +
theme(legend.position="top",
    text = element_text(size=20)) 
#########Automatically filled the box with different color, but the dots only have one color#####


ggplot(data,aes(x=name, y=value,colour=name))+  #Manually set the border colour of box
geom_boxplot(outlier.colour="red", 
               outlier.shape=8,
               outlier.size=3) +  #This script is necessary
scale_color_manual(values = c('red', 'royalblue','green','purple'))+
geom_jitter(size=0.5, aes(color = name)) +
theme(legend.position="top",
      text = element_text(size=20)) 