#####https://statsandr.com/blog/correlation-coefficient-and-correlation-test-in-r/

########Correlation can only be used to measure the LINEAR relationship between to variables!!!!!#####


head(mtcars, 5)
library(tidyverse)
dat <- mtcars %>%
  select(-vs, -am)

###########Correlation coefficient Between two variables######
cor(dat$hp, dat$mpg)
# Spearman correlation between 2 variables
cor(dat$hp, dat$mpg,
    method = "spearman")

#Pearson correlation is often used for quantitative continuous variables that have a linear relationship
#Spearman correlation (which is actually similar to Pearson but based on the ranked values for each variable rather than on the raw data) is 
#often used to evaluate relationships involving at least one qualitative ordinal variable or
#two quantitative variables if the link is partially linear
#Kendall’s tau-b which is computed from the number of concordant and discordant pairs is often used for qualitative ordinal variables


#############Correlation matrix: correlations for all variables########
round(cor(dat),digits = 2)


#############Visualizations################
###1)A scatterplot for 2 variables
library(ggplot2)

ggplot(dat) +
  aes(x = hp, y = mpg) +
  geom_point(colour = "#0c4c8a") +
  theme_minimal()

########Scatterplots for several pairs of variables
pairs(dat[, c("mpg", "hp", "wt")]) #pairs function is from the "graphics" package

#########Another simple correlation matrix#######
library(corrplot)

corrplot(cor(dat),
         method = "number",
         type = "upper")

##################Correlation test#########
##################Correlation test#########
########For 2 variables#######
test <- cor.test(dat$drat, dat$qsec)
test #A pvalue of greater than 0.5 means no linear relationship between two variables

######For several pairs of variables#####
library(Hmisc)
res <- rcorr(as.matrix(dat)) # rcorr() accepts matrices only

round(res$P, 3)

#########Combination of correlation coefficients and correlation tests#######

#######Combine correlation coefficients and correlation tests for TWO pairs of variables#####
library(ggstatsplot)

ggscatterstats(
  data = dat,
  x = wt,
  y = mpg,
  bf.message = FALSE,
  marginal = FALSE # remove histograms
)

#######Combine correlation coefficients and correlation tests for MULTIPLE pairs of variables#####
library(correlation)

correlation::correlation(dat,
                      include_factors = TRUE, method = "auto")
#The correlation only generates a table, not offers the function of visualization

########The following functions can be used to visualize the correlation coefficients and correlation tests results
#of multiple variables
library(GGally) #Approach 1
ggpairs(dat[, c("mpg", "hp", "wt")]) #Approach 1

library(ggstatsplot)#Approach 2
ggcorrmat(
  data = dat[, c("mpg", "hp", "wt")],
  type = "parametric", # parametric for Pearson, nonparametric for Spearman's correlation
  colors = c("darkred", "white", "steelblue") # change default colors
)
#For the 2nd approach, a big cross in the figure indicate the non-significant correlations