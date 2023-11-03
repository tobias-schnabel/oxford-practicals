library(tidyverse)
library(ggplot2)
library(gridExtra)
library(stargazer)
library(kableExtra)
library(MASS)



## Set Paths for tables and figures
root = "/Users/ts/Git/Practicals"
tab = "/Users/ts/Library/CloudStorage/Dropbox/Apps/Overleaf/Practical Template/Tables"
fig = "/Users/ts/Library/CloudStorage/Dropbox/Apps/Overleaf/Practical Template/Figures"

if (getwd() != root) {
  setwd(root)
}
# Load data
data <- read.csv("swim.csv")
## Codebook ##
# For each event, the times of the ﬁnalists are recorded as well as some other 
# information about the event. The variables recorded are:
# • event, the name of the event, e.g. “50 m Freestyle”
# • dist, the length of the event, in metres
# • stroke, the stroke swum in the event
# • sex, to indicate whether an event is women’s or men’s
# • course, to indicate whether an event is short course or long course
# • time, the time of one of the swimmers in the ﬁnal, in seconds.

# Exercise:
#   
# You are asked to investigate how race times depend on the other variables.
# 
# The primary aim of the analysis is: (i) to obtain an interpretable model that 
# explains how time depends on the other variables; and (ii) to interpret the 
# model you obtain.
# You are also asked, using the same model, to predict times for four additional races.

# 1. Perform an exploratory analysis of the data and summarise your ﬁndings. 
# You may wish to consider some numerical summaries as well as some exploratory plots.

head(data)
str(data)

# Sex coded as string, needs to be factor
data$sex <- as.factor(data$sex)

unique(data$course)
# same thing for course
data$course <- as.factor(data$course)

unique(data$stroke)
# repeat for stroke
data$stroke <- as.factor(data$stroke)
str(data)

#repeat for event
data$event <- as.factor(data$event)

# event is concat of dist and stroke, has 
length(unique(data$event)) # 16
# distinct levels, which blows up df used and ruins sensible interpretability
subset(data, dist == 400 & stroke == "Butterfly")
# we also have no current observations for 400m fly, which means we cannot use 
# it to predict anyways

mod0a <- lm(time ~ dist + sex + course + stroke, data = data)
mod0b <- lm(time ~ . , data = data)
mod0c <- lm(time ~ dist*stroke + sex + course, data = data)
stargazer(mod0a, mod0b, mod0c, type = "text",
          single.row = T,
          omit = ".*",
          column.labels = c("Without 'event'", "With 'event'",
                            "with stroke * dist")) 

# dropping event column makes a 
# difference in terms of Adj R2, Residual SE

# this is likely because the event variable has a base level of 50 free, which 
# is captured by the intercept. In the third model, the intercept captures the 
# product of the base levels of both dist and stroke
# mod0b is more flexible, but also more prone to overfitting
# constrains the relationship by assuming that the effect of a particular 
# combination is the sum of the main effects plus an interaction effect. 
# This is less flexible but might be more stable if there are many combinations.
# Indeed we can see that there are 
length(unique(data$dist)) * length(unique(data$stroke)) # 20 possible combinations
# of dist and stroke, but there are only
length(levels(data$event)) # 16 observed combinations

# When we use event as a variable, Only the combinations that exist in the data 
# will have their own coefficient. This means there will be 15 coefficients 
# for the event variable (excluding the reference level).

# When we instead interact dist and stroke, he model attempts to estimate 
# coefficients for all possible interactions, even if certain combinations don't 
# exist in the data. This can result in unstable or non-sensical coefficients for 
# the missing combinations. The behavior of lm in R, in this case, depends on the 
# contrasts used for factors. By default, R uses treatment contrasts, which 
# compare each level to a reference level. If certain interactions don't exist, 
# they won't get their own coefficient, but they can still influence the 
# coefficients of the main effects.

# In conclusion, since we can't use the event variable in prediction later on,
# I drop it here, as optimizing models will lead to inclusion of this variable
# which we cannot use to predict

# New Object for clean data
swim = data[, -1] # drop event for reasons stated above

# Dist is technically numerical but practically has
length(unique(swim$dist)) # 4 distinct levels
# A classical ceteris paribus interpretation also does not really make sense
# as race distances are fixed and "increasing by 1 unit" does not have a sensible
# interpretation

# Check whether it makes a difference for linear modelling
swim$dist <- as.factor(swim$dist)
mod0d <- lm(time ~ dist*stroke + sex + course , data = data[, -1])
mod0e <- lm(time ~ dist*stroke + sex + course , data = swim)
stargazer(mod0d, mod0e, type = "text",
          single.row = T,
          omit = ".*",
          title = "Full Model with 'dist' cast as",
          column.labels = c("Numeric", "Factor")) 

# it does indeed make a (small) difference, factor takes up more df, but negligible

# Summary Table
swim %>%
  group_by(sex, course, stroke) %>%
  summarize(
    Avg_Time = mean(time, na.rm = TRUE),
    SD_Time = sd(time, na.rm = TRUE),
    .groups = 'drop'
  )

stargazer(swim, type = "text")

hist <- ggplot(swim, aes(x = time)) + 
  geom_histogram(binwidth = 1) +
  facet_wrap(~stroke)

box <- ggplot(swim, aes(x = stroke, y = time, fill = sex)) + 
  geom_boxplot() +
  facet_wrap(~course)

point <- ggplot(swim, aes(x = stroke, y = time, color = sex, shape = factor(dist))) +
  geom_point(position = position_dodge(0.8)) +
  facet_wrap(~course) +
  theme_minimal() +
  labs(x = "Stroke", y = "Time (seconds)")

violin1 <- ggplot(swim, aes(x = stroke, y = time, fill = sex)) +
  geom_violin(position = position_dodge(0.8), width = 0.7) + 
  theme_minimal() +
  labs(x = "Stroke", y = "Time (seconds)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("F" = "pink", "M" = "lightblue"))


violin2 <- ggplot(swim, aes(x = stroke, y = time, fill = course)) +
  geom_violin(position = position_dodge(0.8), width = 0.7) + 
  theme_minimal() +
  labs(x = "Stroke", y = "Time (seconds)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("Long" = "lightgreen", "Short" = "cyan"))

grid.arrange(violin1, violin2, ncol=2)

violin3 <- ggplot(swim, aes(x = sex, y = time, fill = course)) +
  geom_violin(position = position_dodge(0.8), width = 0.7) + 
  theme_minimal() +
  labs(x = "Stroke", y = "Time (seconds)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("Long" = "lightgreen", "Short" = "cyan"))



## 2 Model building: 

# establish baseline
mod1a <- lm(time ~ dist*stroke + sex + course , data = swim)
mod1b <- lm(time ~ dist + stroke + sex + course , data = swim)

stargazer(mod1a, mod1b, type = "text",
          single.row = T,
          omit = ".*",
          title = "Baseline: 'dist' and 'factor' included as",
          column.labels = c("Interaction", "Additive")) 
# Interaction better across the board
mod1 <- mod1a # set as baseline

# Diagnostic plots for mod1
par(mfrow = c(2, 2))  
plot(mod1) 
# RvF shows slight pattern
# Q-Q shows deviance in tails
# S-L suggests hetsked

# AIC
step_aic <- stepAIC(mod1, direction = "backward")
# AIC deteriorates when we drop predictors

# BIC
step_bic <- step(mod1, direction = "backward", k = log(nrow(swim)))
# BIC also deteriorates when we drop predictors

# Interaction terms
# based on plot "point", Different strokes have varying times across both short 
# and long distances.The time differences between strokes in short vs. 
# long distances are not consistent. For instance, the time gap for Butterfly 
# between short and long distances is more pronounced than for Backstroke.
mod3 <- lm(time ~ stroke*dist + sex + course, data = swim)
summary(mod3)

# Males and females have different times for the same strokes. For example, in 
# Freestyle and Medley (both short and long), females tend to have slightly 
# longer times than males.
mod4 <- lm(time ~ sex*stroke + course + event, data = swim)
summary(mod4) # some multicoll but better fit

# For certain strokes, the time differences between males and females appear 
# more pronounced in long distances compared to short distances.
mod5 <- lm(time ~ -1 + sex*dist + course + event, data = swim)
summary(mod5) # dropping intercept improves fit

# checking for  nuanced differences in performance across gender, stroke, 
#and distance combine:
mod6 <- lm(time ~ -1 + sex*dist*stroke + course + event, data = swim)
summary(mod6)

mod7 <- lm(time ~ -1 + event*sex*course, data = swim)
summary(mod7)

mod8 <- lm(time ~ -1 + sex*stroke + dist + course, data = swim)
summary(mod8)

mod9 <- lm(time ~ sex*course + dist*stroke, data = swim)
summary(mod9)

# get all models into list
model_list <- mget(grep("^mod[1-9]$", ls(), value = TRUE))

#compare ICs:
sapply(model_list, AIC)
sapply(model_list, BIC)
# model 6 is best in terms of AIC, model 7 in terms of BIC

# However, can't use event as factor because interacting it destroys df and 
# interpretability


# Diagnostic plots for mod6
par(mfrow = c(2, 2))  
plot(mod6)             

# Diagnostic plots for mod7
par(mfrow = c(2, 2))  
plot(mod7)    


# Prediction plot
coef_plot <- ggplot(coef(step_model), aes(x = names(coef(step_model)), y = coef(step_model))) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Side-by-side of baseline and mod7
# Set up plotting parameters
setwd(fig)
png("diag_s_by_s.png", width = 1400, height = 2800, res = 80)

# Adjust layout and margins
par(mfrow = c(8, 2), mar = c(2, 4, 2, 2) + 0.1, oma = c(0, 1, 0, 0), cex = 0.8)

# Plot diagnostics for mod1 and mod7 side by side
plot(mod1, which = 1) # main = "Model 1: Residuals vs Fitted")
plot(mod7, which = 1) #, main = "Model 7: Residuals vs Fitted")
plot(mod1, which = 2) #, main = "Model 1: Normal Q-Q")
plot(mod7, which = 2) #, main = "Model 7: Normal Q-Q")
plot(mod1, which = 3) #, main = "Model 1: Scale-Location")
plot(mod7, which = 3) #, main = "Model 7: Scale-Location")
plot(mod1, which = 5) #, main = "Model 1: Residuals vs Leverage")
plot(mod7, which = 5) #, main = "Model 7: Residuals vs Leverage")
dev.off()



setwd(root)

# Prep reg table with robust SEs for better inference given Heterosked.
library(sandwich)
robustSE7 = vcovHC(mod7)
robustSE1 = vcovHC(mod1)

setwd(tab)
stargazer(mod1, mod7, title="Regression Results", label="tab:results", 
          se=list(sqrt(diag(robustSE1)), sqrt(diag(robustSE7))), 
          type="latex", 
          align=TRUE, 
          column.labels=c("Initial Model", "Final Model"), 
          dep.var.caption="", 
          dep.var.labels.include = FALSE, 
          no.space=TRUE, 
          single.row=TRUE, 
          header=FALSE, 
          digits=3)
          # omit.stat=c("adj.rsq"), #, "f", "ser" 
setwd(root)


# Data for prediction intervals
new_races <- data.frame(
  name = c("RaceA", "RaceB", "RaceC", "RaceD"),
  dist = c(400, 50, 400, 100),
  stroke = c("Freestyle", "Backstroke", "Butterfly", "Medley"),
  sex = rep("F", 4),
  course = rep("Long", 4)
)

# Apply data transformations
# Sex coded as string, needs to be factor
new_races$sex <- as.factor(new_races$sex)
# same thing for course
new_races$course <- as.factor(new_races$course)
# repeat for stroke
new_races$stroke <- as.factor(new_races$stroke)

# New Object for clean data
swim_test = new_races

# Do Prediction
predict(mod8, newdata = swim_test)
