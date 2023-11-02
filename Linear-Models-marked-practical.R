library(tidverse, ggplot2)
library(stargazer, kableExtra)

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
