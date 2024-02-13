#The script is from https://statsandr.com/blog/multiple-linear-regression-made-simple/

library("ggplot2")
library("visreg") #visreg function uses it
library("ggpubr")
library("forcats") #fct_recode in this package
library("performance")#check_model function is in this package
library("see")
library("ggstatsplot") #ggcoefstats function 
library("jtools")
library("ggstance") #plot_summs function uses "jtools" and "ggstance"
library("equatiomatic")#extract_eq() function is in this package


dat <- mtcars


ggplot(dat, aes(x = wt, y = mpg)) +geom_point() +
  labs(y = "Miles per gallon",
       x = "Car's weight (1000 lbs)") +
  theme_minimal()

model <- lm(mpg ~ wt, data = dat)
summary(model)
visreg(model) #The simplest approach to display regression result

#######Another approach to display regression result with regression equation
# and R^2 displayed
ggplot(dat, aes(x = wt, y = mpg)) +
  geom_smooth(method = "lm") +
  geom_point() +
  stat_regline_equation(label.x = 3, label.y = 32) + # for regression equation
  stat_cor(aes(label = after_stat(rr.label)), label.x = 3, label.y = 30) + # for R^2. 
  #These two scripts are from "ggpubr" package
  theme_minimal()
#######Another approach to display regression result with regression equation
# and R^2 displayed

#######Another interpretation of intercept########
dat_centered <- dat
dat_centered$wt_centered <- dat$wt - mean(dat$wt)
mod_centered <- lm(mpg ~ wt_centered,data = dat_centered)

summary(mod_centered)

###########Multiple linear regression######
ggplot(dat) +
  aes(x = wt, y = mpg, colour = hp, size = disp) +
  geom_point() +
  # scale_color_gradient() +
  labs(
    y = "Miles per gallon",
    x = "Weight (1000 lbs)",
    color = "Horsepower",
    size = "Displacement"
  ) +theme_minimal()

model2 <- lm(mpg ~ wt + hp + disp,data = dat)
summary(model2)
check_model(model2)
###########Multiple linear regression######


###########Multiple linear regression with qualitative variables######
dat$vs <- as.character(dat$vs)
dat$vs <- fct_recode(dat$vs,"V-shaped" = "0","Straight" = "1") #fct_recode changes factor levels by hand
model3 <- lm(mpg ~ wt + vs,data = dat)

summary(model3)
###########Multiple linear regression with qualitative variables######

########Based AIC to choose the best model##########
dat$am <- as.character(dat$am)
dat$am <- fct_recode(dat$am,"Automatic" = "0","Manual" = "1")

model4 <- lm(mpg ~ .,data = dat)
model4 <- step(model4, trace = FALSE)
summary(model4)
ggcoefstats(model4)  #This function is from "ggstatsplot" package
########Based AIC to choose the best model##########
model4bis <- lm(mpg ~ wt + qsec + am + hp,data = dat)

plot_summs(model4,model4bis,omit.coefs = NULL) #Display the regression result of multiple models

extract_eq(model4,
           use_coefs = TRUE, # display coefficients
           wrap = TRUE, # multiple lines
           terms_per_line = 2)

########Predictions based on a model##########
predict(model4,
        new = data.frame(wt = 3, qsec = 18, am = "Manual"),
        interval = "confidence",  #predict confidence interval
        # interval = "prediction", #prediction interval is more wider
        level = .95)
########Predictions based on a model##########
#########Overall effect of categorical variables#######
model5 <- lm(mpg ~ vs + am + as.factor(cyl),data = dat)
summary(model5)
Anova(model5) #get the p-value of the overall effect of a categorical variable
#########Overall effect of categorical variables#######
###########Interaction#################
model6 <- lm(mpg ~ wt + am + wt:am,data = dat)
summary(model6)
visreg(model6, "wt", by = "am")
