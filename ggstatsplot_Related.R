###This script is from the help of "ggstatsplot" package
# This package provides "bugs_long","bugs_wide", "iris_long" and "movies_long" data

#As we can see that the "ggstatsplot" package can be used to plot the (1) bar, (2) Box/Violin, (3)Dot-and-whisker
#(4)Visualization of a correlation matrix, (5)Dot plot/chart, (6)Histogram, (7)Pie, (8)Scatterplot and (9)Box/Violin plots for repeated measures comparisons
#plots and their corresponding group plots.

library("ggstatsplot")

###############Single figures###########
#(1) ggbarstats:Stacked bar charts with statistical tests
set.seed(123)
ggbarstats(mtcars, x = vs, y = cyl,results.subtitle = FALSE)


#(2) ggbetweenstats: Box/Violin plots for between-subjects comparisons (IMPORTANT!!!)
set.seed(123)
library(PMCMRplus) # for pairwise comparisons
ggbetweenstats(mtcars, am, mpg)


#(3) ggcoefstats: Dot-and-whisker plots for regression analyses
set.seed(123)
library(lme4)

mod <- lm(formula = mpg ~ cyl * am, data = mtcars)
ggcoefstats(mod)


#(4) ggcorrmat:Visualization of a correlation matrix
set.seed(123)
library(ggcorrplot)
ggcorrmat(iris)

#(5)ggdotplotstats  : Dot plot/chart for labeled numeric data.
set.seed(123)
ggdotplotstats(data = ggplot2::mpg, #the mpg is the sample data offered by ggplot2 with dimention of 234 by 11
  x = cty,
  y = manufacturer,
  title = "Fuel economy data",
  xlab = "city miles per gallon")



#(6) gghistostats: Histogram for distribution of a numeric variable
set.seed(123)

# creating a plot
gghistostats(data = ToothGrowth,
             x = len,
             xlab = "Tooth length",
             centrality.type = "np")


#(7) ggpiestats: Pie charts with statistical tests
ggpiestats(mtcars, vs)

#(8)ggscatterstats: Scatterplot with marginal distributions and statistical results (CORRELATION!!!)
ggscatterstats(
  iris,
  x = Sepal.Width,
  y = Petal.Length,
  label.var = Species,
  label.expression = Sepal.Length > 7.6) +
  ggplot2::geom_rug(sides = "b")

#(9) ggwithinstats:Box/Violin plots for repeated measures comparisons
set.seed(123)
library(dplyr, warn.conflicts = FALSE)

# create a plot
ggwithinstats(data = filter(bugs_long, condition %in% c("HDHF", "HDLF")),
              x    = condition,
              y    = desire,
              type = "np")

###################Grouped plot###########################
###################Grouped plot###########################


#(10) grouped_ggbarstats:Grouped bar charts with statistical tests

set.seed(123)
library(dplyr, warn.conflicts = FALSE)

# let's create a smaller data frame
diamonds_short <- ggplot2::diamonds %>%
  filter(cut %in% c("Very Good", "Ideal")) %>%
  filter(clarity %in% c("SI1", "SI2", "VS1", "VS2")) %>%
  sample_frac(size = 0.05)

# plot
grouped_ggbarstats(
  data          = diamonds_short,
  x             = color,
  y             = clarity,
  grouping.var  = cut,
  plotgrid.args = list(nrow = 2))

#(11)grouped_ggbetweenstats:Violin plots for group or condition comparisons in between-subjects designs 
#repeated across all levels of a grouping variable.
library(PMCMRplus) # for pairwise comparisons
library(dplyr, warn.conflicts = FALSE)
library(ggplot2)

# the most basic function call
grouped_ggbetweenstats(
  data = filter(ggplot2::mpg, drv != "4"),
  x = year,
  y = hwy,
  grouping.var = drv
)

#(12) grouped_ggcorrmat: Visualization of a correlalogram (or correlation matrix) 
#for all levels of a grouping variable
set.seed(123)

grouped_ggcorrmat(
  data = iris,
  grouping.var = Species,
  type = "robust",
  p.adjust.method = "holm",
  plotgrid.args = list(ncol = 1L),
  annotation.args = list(tag_levels = "i"))

#(13) grouped_ggdotplotstats: Grouped histograms for distribution of a labeled numeric variable
set.seed(123)
library(dplyr, warn.conflicts = FALSE)

# removing factor level with very few no. of observations
df <- filter(ggplot2::mpg, cyl %in% c("4", "6", "8"))


grouped_ggdotplotstats(
  data         = df,
  x            = cty,
  y            = manufacturer,
  grouping.var = cyl,
  test.value   = 15.5)

#(14) grouped_gghistostats:Grouped histograms for distribution of a numeric variable
set.seed(123)

# plot
grouped_gghistostats(
  data            = iris,
  x               = Sepal.Length,
  test.value      = 5,
  grouping.var    = Species,
  plotgrid.args   = list(nrow = 1),
  annotation.args = list(tag_levels = "i")
)

#(15)grouped_ggpiestats: Grouped pie charts with statistical tests
set.seed(123)
grouped_ggpiestats(mtcars, x = cyl, grouping.var = am)

#(16)grouped_ggscatterstats:Scatterplot with marginal distributions for all levels of a grouping variable
set.seed(123)

library(dplyr, warn.conflicts = FALSE)
library(ggplot2)

grouped_ggscatterstats(
  data             = filter(movies_long, genre == "Comedy" | genre == "Drama"),
  x                = length,
  y                = rating,
  type             = "robust",
  grouping.var     = genre,
  ggplot.component = list(geom_rug(sides = "b"))
)

#(17)grouped_ggwithinstats: Violin plots for group or condition comparisons in 
#within-subjects designs repeated across all levels of a grouping variable.
set.seed(123)
library(dplyr, warn.conflicts = FALSE)
library(ggplot2)

# the most basic function call
grouped_ggwithinstats(
  data             = filter(bugs_long, condition %in% c("HDHF", "HDLF")),
  x                = condition,
  y                = desire,
  grouping.var     = gender,
  type             = "np",
  ggplot.component = scale_y_continuous(breaks = seq(0, 10, 1), limits = c(0, 10))
)
