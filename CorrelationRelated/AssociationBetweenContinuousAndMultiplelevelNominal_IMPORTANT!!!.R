######Association for one continuous and one multiple-level nominal variables ###
#####ANOVA: https://statsandr.com/blog/anova-in-r/#fn2
####kruskal-wallis:https://statsandr.com/blog/kruskal-wallis-test-nonparametric-version-anova/

library("palmerpenguins")
library("tidyverse")

dat <- penguins %>%
  select(species, flipper_length_mm)

#######Visualize the data#####
ggplot(dat) +
  aes(x = species, y = flipper_length_mm, color = species) +
  geom_jitter() +
  theme(legend.position = "none")
#######Visualize the data#####
#Different from the two-level t-test, we perform normality test on two groups of data
#For the multiple level test, we perform the normality test on the resisual of aov, as presented below
res_aov <- aov(flipper_length_mm ~ species,
               data = dat)

#########Check the normality#######
shapiro.test(res_aov$residuals) #Test the normality of residual.Very strict criterion
hist(res_aov$residuals)#Appropriate approach
#If the data follow the normal distribution, we further test the variance between groups by using the subsequent "leveneTest"..
#If the data does not follow the normal distribution, jump to the Kruskal-Wallis test####
#########Check the normality#######

#########Check the variance#######
boxplot(flipper_length_mm ~ species,
        data = dat) ##Vidually check the variance

library("car")

leveneTest(flipper_length_mm ~ species,
           data = dat) #leveneTest is from "car" package
#The var.test can only be used for two levels data; 
#for data with more than 2 levels, use leveneTest
#!!!!For a Pr>0.05, we conclude that the variances are equal between species 
#########Check the variance#######

######If the variance of groups are equal, do one-way anova by using "aov"####
res_aov <- aov(flipper_length_mm ~ species,
               data = dat)
summary(res_aov) ##Return 14 lists

########Post-hoc tests in R and their interpretation###
##############1)Tukey HSD test##############
library("multcomp")
post_test <- glht(res_aov,
                  linfct = mcp(species = "Tukey")) #Multiple Comparisons of Means: Tukey Contrasts

summary(post_test)

TukeyHSD(res_aov) #Another method to do the Tukey HSD test

post_test <- glht(res_aov,
                  linfct = mcp(species = "Dunnett"))
summary(post_test)
#Instead of Tukey HSD test,the Dunnett can be used to make comparisons with a reference group
###If the variance of groups are not equal, perform welch anova####

pairwise.t.test(dat$flipper_length_mm, dat$species,
                p.adjust.method = "holm") #From {stats} package
#By reducing the number of post-hoc comparisons to what is necessary only, and no more, you maximize the statistical power


#############Visualization of ANOVA and post-hoc tests on the same plot##########
library("ggstatsplot")

ggbetweenstats(
  data = dat,
  x = species,
  y = flipper_length_mm,
  type = "parametric", # ANOVA 
  var.equal = TRUE, # ANOVA or Welch ANOVA. The default setting is "FALSE"
  plot.type = "box",
  pairwise.comparisons = TRUE,
  pairwise.display = "significant",
  centrality.plotting = FALSE,
  # p.adjust.method = "hochberg",
  bf.message = FALSE)

##################Perform welch anova when the variance are non-equal#################
######Approach 1#########
oneway.test(flipper_length_mm ~ species,
            data = dat,
            var.equal = TRUE) # assuming equal variances

######Approach 2#########
library("rstatix")
res_aov <- welch_anova_test(data=dat,flipper_length_mm ~ species)

########Post-hoc tests in R for non-equal variance###
##############1)Games-Howell test##############
games_howell_test(dat, flipper_length_mm~species, conf.level = 0.95, detailed = FALSE)

ggbetweenstats(
  data = dat,
  x = species,
  y = flipper_length_mm,
  type = "parametric", # ANOVA 
  var.equal = FALSE, #  Welch ANOVA
  plot.type = "box",
  pairwise.comparisons = TRUE,
  pairwise.display = "significant",
  centrality.plotting = FALSE,
  bf.message = FALSE)
##################################################################
##################################################################
########!!!!If the data is not normally distributed, the Kruskal-Wallis test is needed#######
kruskal.test(flipper_length_mm~species, data = dat) #From "stats" package
####A p-value<0.05, indicates that there is significant difference between the means of each group.
pairwise.wilcox.test(dat$flipper_length_mm, dat$species,
                     p.adjust.method = "BH")
##The p.adjust.method can be c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY",
#   "fdr", "none")

ggbetweenstats(
  data = dat,
  x = species,
  y = flipper_length_mm,
  type = "nonparametric", # Kruskal-Wallis
  var.equal = FALSE, #When the type="nonparametric", var.equal=TRUE or FALSE gives the same results.
  plot.type = "box",
  pairwise.comparisons = TRUE,
  pairwise.display = "significant",
  centrality.plotting = FALSE,
  p.adjust.method = "holm",
  bf.message = FALSE)
