#Automatically calculate percentage by a factor:
#https://statisticsglobe.com/calculate-percentage-group-r

data <- data.frame(group = rep(LETTERS[1:3], each = 4),  # Create example data
                   subgroup = letters[1:4],
                   value = 1:12)
data 

#approach 1 : transform
data_new1 <- transform(data,                             # Calculate percentage by group
                       perc = ave(value,
                                  group,
                                  FUN = prop.table))
data_new1 

#approach 2: dplyr
library("dplyr")
data_new2 <- data %>%                                    # Calculate percentage by group
  group_by(group) %>%
  mutate(perc = value / sum(value)) %>% 
  as.data.frame()
data_new2 