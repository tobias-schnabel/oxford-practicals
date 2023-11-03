library(tidyverse)
library(ggplot2)
library(stargazer)
library(kableExtra)
library(MASS)

data <- read.csv("swim.csv")

## Set Paths for tables and figures
tab = "/Users/ts/Library/CloudStorage/Dropbox/Apps/Overleaf/Practical Template/Tables"
fig = "/Users/ts/Library/CloudStorage/Dropbox/Apps/Overleaf/Practical Template/Figures"

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

# New Object for clean data
swim = data

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

point <- ggplot(swim, aes(x = stroke, y = time, color = sex)) +
  geom_point(position = position_dodge(0.8)) +
  facet_wrap(~course) +
  theme_minimal() +
  labs(x = "Stroke", y = "Time (seconds)")

interact <- ggplot(swim, aes(x = dist, y = time, color = stroke)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE)

## 2 Model building
mod1 <- lm(time ~ dist + sex + course + stroke + event, data = swim)
summary(mod1)

# Diagnostic plots for mod1
par(mfrow = c(2, 2))  
plot(mod1) 
# RvF shows slight pattern
# Q-Q shows deviance in tails
# S-L suggests hetsced

# AIC
step_aic <- stepAIC(mod1, direction = "both")
summary(step_aic)
# results are  favor of dropping dist and stroke

# BIC
n <- nrow(swim)
step_bic <- step(mod1, direction = "both", k = log(n))
# results are  favor of dropping dist and stroke
# this makes intuitive sense, all information contained in both dist and stroke 
# is contained in event already
mod2 <- lm(time ~ sex + course + event, data = swim)

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

# get all models into list
model_list <- mget(grep("^mod[1-8]$", ls(), value = TRUE))

#compare ICs:
sapply(model_list, AIC)
sapply(model_list, BIC)
# model 6 is best in terms of AIC, mod7 in terms of BIC


# Diagnostic plots for mod6
par(mfrow = c(2, 2))  
plot(mod6)             

# Diagnostic plots for mod7
par(mfrow = c(2, 2))  
plot(mod7)    

# SIde-by-side of baseline and mod7
# Set up plotting parameters
setwd(fig)
# png("diag_s_by_s.png", width = 1000, height = 500, res = 500)
# par(mfrow = c(8, 2), oma = c(1, 1, 1, 0), mar = c(4, 4, 2, 2) + 0.1)
png("diag_s_by_s.png", width = 1400, height = 1800, res = 100)

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


