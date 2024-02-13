######Association for one continuous and one two-level nominal variables ###
######https://www.crispanalytics.io/post/what-type-of-correlation-is-appropriate-for-nominal-and-continuous-data
######http://www.sthda.com/english/wiki/comparing-means-in-r
#!!!!The script is mainly based on: https://statsandr.com/blog/student-s-t-test-in-r-and-by-hand-how-to-compare-two-groups-under-different-scenarios/
####Pay special attention to "Different versions of the Student’s t-test" section. Though 5 scenarios were offered,
#we focus on the scenarios 2,3 and  5, because  the variances of the populations are most of the time unknown in practice.


######Assumptions for t-test#####
#1.Independence
#2.Normality (when sample size in either group <30, observations in both groups should follow a normal distribution)
#3.Equality of variances (var.test).If the hypothesis of equal variances is rejected, another version of the Student’s t-test can be used, Welch test (t.test(variable ~ group, var.equal = FALSE))
#4.Outlier: either transform the data or remove them.
library("ggstatsplot")
library("ggplot2")

#########Scenario 2: Independent samples with 2 equal but unknown variances#####3

dat2 <- data.frame(
  value = c(1.78, 1.5, 0.9, 0.6, 0.8, 1.9, 0.8, -0.7, -0.1, 0.4, 0.1),
  sample = c(rep("1", 6), rep("2", 5)))

ggplot(dat2) +
  aes(x = sample, y = value) +
  geom_boxplot() +
  theme_minimal()

ggbetweenstats(
  data = dat2,
  x = sample,
  y = value,
  plot.type = "box", # for boxplot
  type = "parametric", # for student's t-test
  var.equal = TRUE, # equal variances
  centrality.plotting = FALSE # remove mean
) +
  labs(caption = NULL) # remove caption

with(dat2, shapiro.test(value[sample=="1"])) #A p-value>0.05, we can conclude the data follow normal distribution
with(dat2, shapiro.test(value[sample=="2"]))
var.test(value ~ sample, data = dat2)
#A p-value>0.05,there is no significant difference between the variances of the two sets of data

test <- t.test(value ~ sample,
               data = dat2,
               var.equal = TRUE,  ###Two sets of data have the same variance
               alternative = "greater") #Using this formula format is more convenient
test #a p-value less than 0.05 indicates that the means of these two sets of data are significantly different


#######Scenario 3: Independent samples with 2 unequal and unknown variances######
dat3 <- data.frame(
  value = c(0.8, 0.7, 0.1, 0.4, 0.1, 1.78, 1.5, 0.9, 0.6, 0.8, 1.9),
  sample = c(rep("1", 5), rep("2", 6))
)
dat3

ggbetweenstats(
  data = dat3,
  x = sample,
  y = value,
  plot.type = "box", # for boxplot
  type = "parametric", # for student's t-test
  var.equal = FALSE, # unequal variances
  centrality.plotting = FALSE # remove mean
) +
  labs(caption = NULL) # remove caption

with(dat3, shapiro.test(value[sample=="1"])) #A p-value>0.05, we can conclude the data follow normal distribution
with(dat3, shapiro.test(value[sample=="2"]))
var.test(value ~ sample, data = dat3)
#A p-value>0.05,there is no significant difference between the variances of the two sets of data.
#So this set of data might not be a good example for 2 unequal and unknown variances
test <- t.test(value ~ sample,
               data = dat3,
               var.equal = FALSE,  #Two sets of data with non-equal variance
               alternative = "less")
test

########Scenario 5: Paired samples where the variance of the differences is unknown###
dat5 <- data.frame(
  value = c(9, 8, 1, 3, 2, 16, 11, 15, 12, 9),
  time = c(rep("before", 5), rep("after", 5))
)
dat5

ggwithinstats( ##For paired sample, "ggwithinstats" rather than "ggbetweenstats" was used
  data = dat5,
  x = time,
  y = value,
  type = "parametric", # for student's t-test
  centrality.plotting = FALSE # remove mean
) +
  labs(caption = NULL) # remove caption

test <- t.test(value ~ time,
               data = dat5,
               alternative = "greater",
               paired = TRUE) ##specified pair test
test
#######################################################3
#######If the data does not follow normal distribution,
#The Mann-Withney-Wilcoxon test (also referred as Wilcoxon rank sum test or Mann-Whitney U test) is performed for Independent samples;
#Wilcoxon signed-rank test (also sometimes referred as Wilcoxon test for paired samples) is performed when the samples are paired/dependent

#######Independent samples#######
dat <- data.frame(
  Sex = as.factor(c(rep("Girl", 12), rep("Boy", 12))),
  Grade = c(19, 18, 9, 17, 8, 7, 16, 19, 20, 9, 11, 18,
            16, 5, 15, 2, 14, 15, 4, 7, 15, 6, 7, 14)
)

dat

ggbetweenstats( # independent samples
  data = dat,
  x = Sex,
  y = Grade,
  plot.type = "box", # for boxplot
  type = "nonparametric", # for wilcoxon
  centrality.plotting = FALSE # remove median
)

shapiro.test(subset(dat, Sex == "Girl")$Grade)
shapiro.test(subset(dat, Sex == "Boy")$Grade)
wilcox.test(dat$Grade ~ dat$Sex)

############Paired samples#######
dat <- data.frame(
  Beginning = c(16, 5, 15, 2, 14, 15, 4, 7, 15, 6, 7, 14),
  End = c(19, 18, 9, 17, 8, 7, 16, 19, 20, 9, 11, 18)
)

dat

dat2 <- data.frame(
  Time = c(rep("Before", 12), rep("After", 12)),
  Grade = c(dat$Beginning, dat$End)
)
dat2

ggwithinstats( # paired samples
  data = dat2,
  x = Time,
  y = Grade,
  type = "nonparametric", # for wilcoxon
  centrality.plotting = FALSE # remove median
)

wilcox.test(dat2$Grade ~ dat2$Time,
            paired = TRUE) ##Specified paired test
test