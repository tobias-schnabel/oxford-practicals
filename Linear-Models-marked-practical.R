library(tidyverse)
library(ggplot2)
library(gridExtra)
library(patchwork) # to combine plots
library(stargazer)
library(kableExtra)
library(sandwich) # for robust Standard Errors
library(MASS) # for stepwise AIC and BIC selection
library(ggfortify)

## Set Paths for tables and figures
root = "/Users/ts/Git/Practicals"
tab = "/Users/ts/Library/CloudStorage/Dropbox/Apps/Overleaf/LM Practicals/Tables"
fig = "/Users/ts/Library/CloudStorage/Dropbox/Apps/Overleaf/LM Practicals/Figures"
code = "/Users/ts/Library/CloudStorage/Dropbox/Apps/Overleaf/LM Practicals/Code"

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

## EDA
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


## Plots
hist <- ggplot(swim, aes(x = time)) + 
  geom_histogram(binwidth = 1) +
  facet_wrap(~stroke)

box <- ggplot(swim, aes(x = stroke, y = time, fill = sex)) + 
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~course)

point <- ggplot(swim, aes(x = stroke, y = time, color = sex, shape = factor(dist))) +
  geom_point(position = position_dodge(0.8)) +
  facet_wrap(~course) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
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

violins = grid.arrange(violin1, violin2, ncol=2)

violin3 <- ggplot(swim, aes(x = sex, y = time, fill = course)) +
  geom_violin(position = position_dodge(0.8), width = 0.7) + 
  theme_minimal() +
  labs(x = "Stroke", y = "Time (seconds)") +
  scale_fill_manual(values = c("Long" = "lightgreen", "Short" = "cyan"))

# Collect in list
plotlist <- list(hist, box, point, violins, violin3)

# Dist is technically numerical but practically has
length(unique(swim$dist)) # 4 distinct levels
# A classical ceteris paribus interpretation also does not really make sense
# as race distances are fixed and "increasing by 1 unit" does not have a sensible
# interpretation

# Check whether it makes a difference for linear modelling
swim$dist <- as.factor(swim$dist)
mod0d <- lm(time ~ dist*stroke + sex + course , data = data[, -1])
mod0e <- lm(time ~ dist*stroke + sex + course , data = swim)
mod0d$AIC <- AIC(mod0d)
mod0e$AIC <- AIC(mod0e)
stargazer(mod0d, mod0e, type = "text",
          single.row = T,
          omit = ".*",
          title = "Full Model with 'dist' cast as",
          column.labels = c("Numeric", "Factor"),
          keep.stat = c("n", "rsq", "adj.rsq", "ser","f",  "aic")) 

# it does indeed make a (small) difference, factor takes up more df, but negligible

## 2 Model building: 

# establishing baseline
mod1 <- lm(time ~ dist*stroke + sex + course , data = swim)
mod2 <- lm(time ~ dist + stroke + sex + course , data = swim)

mod1$AIC <- AIC(mod1)
mod2$AIC <- AIC(mod2)

stargazer(mod1, mod2, type = "text",
          single.row = T,
          omit = ".*",
          title = "Baseline: 'dist' and 'factor' included as",
          column.labels = c("Interaction", "Additive"),
          keep.stat = c("n", "rsq", "adj.rsq", "ser","f",  "aic")) 
# Interaction better across the board


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

# Males and females have different times for the same strokes. For example, in 
# Freestyle and Medley (both short and long), females tend to have slightly 
# longer times than males.
mod3 <- lm(time ~ sex*stroke + dist + course, data = swim)

# For certain strokes, the time differences between males and females appear 
# more pronounced in long distances compared to short distances.
mod4 <- lm(time ~ sex*dist + course + stroke, data = swim)

# create new variable that captures lengths swum
swim$lengths <- ifelse(swim$course == "Short", 
                       as.numeric(as.character(swim$dist)) / 25, 
                       as.numeric(as.character(swim$dist)) / 50)

mod5 <- lm(time ~ sex * stroke + lengths, data = swim)
mod6 <- lm(time ~ dist * stroke + lengths*sex, data = swim)

# get all models into list
model_list <- mget(grep("^mod[1-9]$", ls(), value = TRUE))

#compare ICs:
aic <- round(sapply(model_list, AIC), 2)
bic <- round(sapply(model_list, BIC), 2)
which(aic ==min(aic))
which(bic ==min(bic))
# model 6 is best in terms of AIC and BIC
# get number of coefficients
p <- sapply(model_list, function(mod) length(coef(mod)))

# Make small table
icmat = rbind(p, aic, bic)
colnames(icmat) <- paste0("Model ", 1:6)
icmat[1, ] <- as.integer(icmat[1, ])
ictable <- kable(icmat, "latex", booktabs = TRUE, digits = 2) %>%
  row_spec(0, bold = TRUE) # Make the header row bold
# kable_styling(latex_options = c("striped", "scale_down")) %>%



# However, can't use event as factor because interacting it destroys df and 
# interpretability


# Side-by-side of mod4 and mod6 diagnostic plots
diag1_1 <- autoplot(mod1, which = 1)
diag1_2 <- autoplot(mod1, which = 2)
diag1_3 <- autoplot(mod1, which = 5)
diag1_4 <- autoplot(mod1, which = 6)

diag4_1 <- autoplot(mod4, which = 1)
diag4_2 <- autoplot(mod4, which = 2)
diag4_3 <- autoplot(mod4, which = 5)
diag4_4 <- autoplot(mod4, which = 6)

# Arrange side by side
diag_plot <- diag1_1 + diag4_1 + diag1_2 + diag4_2 +
  diag1_3 + diag4_3 + diag1_4 + diag4_4 +
  plot_layout(ncol = 2, nrow = 4)

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
# repeat for dist
new_races$dist <- as.factor(new_races$dist)

# New Object for clean data
swim_test = new_races

# Do Prediction
pred_df <- data.frame(
  predict(mod4, newdata = swim_test, interval = "prediction")
) %>% mutate(across(everything(), ~ round(., 2)))


## Export tables
setwd(tab)
save_kable(ictable, file = "ictable.tex")

kable(pred_df, "latex", booktabs = TRUE, caption = "Prediction Intervals",
      col.names = c("Prediction", "Lower Bound", "Upper Bound")) %>%
  column_spec(1, bold = TRUE) %>%  
  save_kable(file = "predictions.tex")

stargazer(mod0a, mod0b, mod0c, out = "event.tex",
          single.row = T,
          omit = ".*",
          title = "Inclusion of 'event' variable",
          column.labels = c("Without 'event'", "With 'event'",
                            "with stroke * dist")) 

stargazer(mod0d, mod0e, out = "dist.tex",
          single.row = T,
          omit = ".*",
          title = "Full Model with 'dist' cast as",
          column.labels = c("Numeric", "Factor"),
          keep.stat = c("n", "rsq", "adj.rsq", "ser","f",  "aic")) 

stargazer(mod1, mod2, out = "dist_stroke.tex",
          single.row = T,
          omit = ".*",
          title = "Baseline: 'dist' and 'stroke' included as",
          column.labels = c("Interaction", "Additive"),
          keep.stat = c("n", "rsq", "adj.rsq", "ser","f",  "aic"))

# Prep reg table with robust SEs for better inference given Heterosked.
robustSE6 = vcovHC(mod6)
robustSE4 = vcovHC(mod4)
mod6$AIC <- AIC(mod6)
mod4$AIC <- AIC(mod4)

stargazer(mod4, mod6, title="Regression Results", 
          se=list(sqrt(diag(robustSE4)), sqrt(diag(robustSE6))), 
          type="latex", 
          align=TRUE, 
          out = "regtable.tex",
          column.labels=c("Model 4", "Model 6"), 
          dep.var.caption="", 
          dep.var.labels.include = FALSE, 
          no.space=TRUE, 
          single.row=TRUE, 
          header=FALSE, 
          digits=3,
          keep.stat = c("n", "rsq", "adj.rsq", "ser","f",  "aic"))

# just model 4
stargazer(mod4, title="Regression Results", 
          se=list(sqrt(diag(robustSE4))), 
          type="latex", 
          align=TRUE, 
          out = "interpretation.tex",
          column.labels=c("Model 4"), 
          dep.var.caption="", 
          dep.var.labels.include = FALSE, 
          no.space=TRUE, 
          single.row=TRUE, 
          header=FALSE, 
          digits=3,
          keep.stat = c("n", "rsq", "adj.rsq", "ser","f",  "aic"))
# omit.stat=c("adj.rsq"), #, "f", "ser" 
setwd(root)


## Export figures
setwd(fig)
# export side-by-side diagnostics plos
png("diagnostics.png", width = 4000, height = 4000, res = 200)
print(diag_plot)
dev.off()

# export ggplots
# Loop through the list of plots
for (i in seq_along(plotlist)) {
  if (!is.null(plotlist[[i]])) { # Check if the plot is not null
    # Construct the filename using the index
    filename <- paste0("plot", i, ".png")
    # Save the plot to disk
    ggsave(plot = plotlist[[i]], filename = filename)
  }
}

setwd(code)
file.copy(from = "/Users/ts/Git/Practicals/Linear-Models-marked-practical.R",
          to = "/Users/ts/Library/CloudStorage/Dropbox/Apps/Overleaf/LM Practicals/Code",
          overwrite = T)
setwd(root)
