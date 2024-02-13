#This scripture is used to draw the histogram and densityplot by using ggplot2 package

library("ggplot2")
library("dyplr")

set.seed(1234)
df <- data.frame(
  sex=factor(rep(c("F", "M"), each=200)),
  weight=round(c(rnorm(200, mean=55, sd=5), rnorm(200, mean=65, sd=5)))
)
head(df)

p<-ggplot(df, aes(x=weight)) + 
  geom_histogram(color="black", fill="grey",binwidth=1)
p

# Add mean line
p+ geom_vline(aes(xintercept=mean(weight)),
              color="blue", linetype="dashed", linewidth=1)

# Histogram with density plot
ggplot(df, aes(x=weight)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white",linetype="dashed")+ #y-axis is the density not the count
  geom_density(alpha=.2, fill="#FF6666") #"alpha decides the transparency

# Calculate mean by group
mu <- df %>% group_by(sex) %>% 
  summarise(mean_weight=mean(weight)) #summarise is from dplyr

p<-ggplot(df, aes(x=weight, color=sex,fill=sex)) +
  geom_histogram(position="dodge",alpha=0.5)+
  geom_vline(data=mu, aes(xintercept=mean_weight, color=sex),
             linetype="dashed",linewidth=2)+
  theme(legend.position="top")+facet_grid(sex ~ .) #The facet_grid is used to gegnerate separate histgram plot for each group
p


