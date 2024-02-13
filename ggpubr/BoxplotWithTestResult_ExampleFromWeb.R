#The script is from the following website:
#http://www.sthda.com/english/articles/24-ggpubr-publication-ready-plots/76-add-p-values-and-significance-levels-to-ggplots/

library("ggpubr")
data("ToothGrowth")
compare_means(len ~ supp, data = ToothGrowth,method = "wilcox.test")

#####Add test result to ggplot (for two groups)######
p <- ggboxplot(ToothGrowth, x = "supp", y = "len",
               color = "supp", palette = "jco",
               add = "jitter")
#  Add p-value
p + stat_compare_means()
# Change method
p + stat_compare_means(method = "t.test")
#####Add test result to ggplot (for two groups)######

#######Visualize paired data######
ggpaired(ToothGrowth, x = "supp", y = "len",
         color = "supp", line.color = "gray", line.size = 0.4,
         palette = "jco")+
  stat_compare_means(paired = TRUE)
#######Visualize paired data######

#########Compare more than two groups, but only display global p-value####
# Default method = "kruskal.test" for multiple groups
ggboxplot(ToothGrowth, x = "dose", y = "len",
          color = "dose", palette = "jco")+
  stat_compare_means()
# Change method to anova
ggboxplot(ToothGrowth, x = "dose", y = "len",
          color = "dose", palette = "jco")+
  stat_compare_means(method = "anova")
#########Compare more than two groups, but only display global p-value####

######## Visualize: Specify the comparisons you want#####
my_comparisons <- list( c("0.5", "1"), c("1", "2"), c("0.5", "2") )
ggboxplot(ToothGrowth, x = "dose", y = "len",
          color = "dose", palette = "jco")+ 
  stat_compare_means(comparisons = my_comparisons)+ # Add pairwise comparisons p-value
  stat_compare_means(label.y = 50)     # Add global p-value

#######Multiple pairwise tests against a reference group##
ggboxplot(ToothGrowth, x = "dose", y = "len",
          color = "dose", palette = "jco")+
  stat_compare_means(method = "anova", label.y = 40)+      # Add global p-value
  stat_compare_means(label = "p.signif", method = "t.test",
                     ref.group = "0.5") 

#######Multiple pairwise tests against base-mean#####
ggboxplot(ToothGrowth, x = "dose", y = "len",
          color = "dose", palette = "jco")+
  stat_compare_means(method = "anova", label.y = 40)+      # Add global p-value
  stat_compare_means(label = "p.signif", method = "t.test",
                     ref.group = ".all.")  

myeloma <- read.delim("https://raw.githubusercontent.com/kassambara/data/master/myeloma.txt")
# Perform the test
compare_means(DEPDC1 ~ molecular_group,  data = myeloma,
              ref.group = ".all.", method = "t.test")

ggboxplot(myeloma, x = "molecular_group", y = "DEPDC1", color = "molecular_group", 
          add = "jitter", legend = "none") +
  rotate_x_text(angle = 45)+
  geom_hline(yintercept = mean(myeloma$DEPDC1), linetype = 2)+ # Add horizontal line at base mean
  stat_compare_means(method = "anova", label.y = 1600)+        # Add global annova p-value
  stat_compare_means(label = "p.signif", method = "t.test",
                     ref.group = ".all.",hide.ns = TRUE)                      # Pairwise comparison against all

##########Multiple grouping variables#########
#####Two independent sample comparisons after grouping the data by another variable#####
# Box plot facetted by "dose"
p <- ggboxplot(ToothGrowth, x = "supp", y = "len",
               color = "supp", palette = "jco",
               add = "jitter",facet.by = "dose", 
               short.panel.labs = FALSE)
# Use only p.format as label. Remove method name.
p + stat_compare_means(label = "p.format")

########Create one single panel with all box plots. Plot y = “len” by x = “dose” and color by “supp”###
p <- ggboxplot(ToothGrowth, x = "dose", y = "len",
               color = "supp", palette = "jco",
               add = "jitter")
p + stat_compare_means(aes(group = supp))

