# non-assessed practical week 3 MT23

# The data set we are considering today describes a cloud seeding experiment aimed 
# at increasing rainfalls, taken from Cook and Weisberg’s “Residuals and Inference 
# in Regression” book. It used silver iodide as a catalyst to induce rain, and 
# targeted an area of 3000 square miles north-east of Coral Gable, California 
# for 24 days in the summer of 1975. The following variables were recorded:
library(tidyverse, ggplot2, stargazer)
data <- read_table("cloud.seeding.txt")

#codebook
# Action (A): a classiﬁcation indicating seeding (coded 1) or no seeding (coded 0).
# Time (T): days after the beginning of the experiment.
# Suitability (SNe): if SNe > 1.5 the day was judged suitable for seeding based on natural conditions.
# Echo coverage (C): per cent cloud cover in the area, measured using radar.
# Pre-wetness (P): total rainfall in the target area
# Echo motion (E): a classiﬁcation indicating a moving radar echo (coded 1) or a 
# stationary radar echo (coded 2).
# 
# Response (Y): amount of rain (in 10 7m 3) that fell in the area for a 6-hours 
# period on each suitable day

head(data)
str(data)

##1
#2
# Which variables appear to be related to the response variable, and thus may 
# be good choices for an explanatory variable in a linear model? [Use cor().]
# a priori:
# suitability (SNe) makes sense, unlikely to have seeding if judged not suitable
# pre-wetness: aim of seeding is to increase rainfall
# A: whether there was seeding or not (hopefully) makes a difference in rainfall
cor(data)
# T, SNe, E, C hae correlations ≥0.3, O, A, P has smaller

# Perform a graphical inspection of the relationship between the response Y 
# and the other variables. Does any variable show a deﬁnite trend?
par(mfrow = c(2, 3))
plot(data$Y, data$SNe, main = "SNe")
plot(data$Y, data$E, main = "E")
plot(data$Y, data$C, main = "C")
plot(data$Y, data$A, main = "A")
plot(data$Y, data$P, main = "P")

# somehwhat linear trend in SNe given Y, which given high correlation is expected

# Transform A and E into factors with as.factor(). 
# Is Y distributed diﬀerently for the level of each of these variables?
library(dplyr)

dat <- data %>%
  mutate(A = factor(A))%>% 
  mutate(E = as.factor(E))

# Boxplot for 'A'
ggplot(dat, aes(x = A, y = Y, fill = A)) +
  geom_boxplot() +
  labs(title = "Distribution of Y across levels of A", x = "A", y = "Y") +
  theme_minimal()

# Y has a wider range for A=0 (no seeding)

# Boxplot for 'E'
ggplot(dat, aes(x = E, y = Y, fill = E)) +
  geom_boxplot() +
  labs(title = "Distribution of Y across levels of E", x = "E", y = "Y") +
  theme_minimal()
# Y has a slightly wider and generally lower range for E=1 (moving radar echo)

## Ex 2
# 1. Fit a simple linear regression using Y as the response variable and T; 
# save the model in an object called mT; and extract regression coeﬃcients, 
# residuals and ﬁtted values.

mT <- lm(Y ~ T, data = dat)
mT_coefs <- mT$coefficients
mT_resids <- mT$residuals
mT_fitted <- mT$fitted.values

stargazer(mT, type = "text")
# 2. Describe the main quantities present in the output of summary(mT).
# Time, negative coefficient, stat. sig. at 5%
# R2 of ca 25%

# 3. Is there any evidence that the rainfalls are increasing with time? 
#   Use the regression coeﬃcient for T to assess whether there is any 
# signiﬁcant relationship between Y and T.
# yes, at the 5% sig level, rainfall decreases in the number of days since 
#the beginning of the experiment

# 4. Now perform a simple linear regression using ﬁrst C, and then P, 
# and save them respectively as mC and mP. 
# Are the respective regression coeﬃcients signiﬁcant?
mC <- lm(Y ~ C, data = dat)
stargazer(mC, type = "text")
# not for C
mP <- lm(Y ~ P, data = dat)
stargazer(mP, type = "text")
# not for P either

# Try a few transformations of C, such as log(C) and C 2, and then do the 
# same for of P; does the model ﬁt the data any better? 
mc_log <- lm(Y ~ log(C), data = dat)
stargazer(mc_log, type = "text") # log C somewhat better,
mc_sq <- lm(Y ~ I(C^2), data = dat)
stargazer(mc_sq, type = "text") # C squared not
mc_cu <- lm(Y ~ I(C^3), data = dat)
stargazer(mc_cu, type = "text") # C cubed considerably worse

mp_log <- lm(Y ~ log(P), data = dat)
stargazer(mp_log, type = "text") # log P not better,
mp_sq <- lm(Y ~ I(P^2), data = dat)
stargazer(mp_sq, type = "text") # P not better either
mp_cu <- lm(Y ~ I(P^3), data = dat)
stargazer(mp_cu, type = "text") # P cubed considerably worse


# Does it make sense to compare models after transforming the explanatory 
# variable? [Consider R 2 values.]
# sure, scaling of dependent variable stays the same

# Now transform Y into log(Y) and ﬁt a simple linear regression using 
# C as the explanatory variable. Does it make sense to compare 
# (using R 2 , or the residual standard error) how this model ﬁts 
# compared to previous models?
mC_logy <- lm(log(Y) ~ C, data = dat)
stargazer(mC_logy, type = "text")

# no, scaling changes, so variation changes and comparing R2 is meaningless

# Fit a multiple linear regression with Y as the response and T, C and P 
# as explanatory variables, and save it into an object called mCPT. 
# Are the regression coeﬃcients the same as in the simple linear regressions 
# ﬁtted above? Why?
mCPT <- lm(Y ~ T + C + P, data = dat)
stargazer(mCPT, type = "text")
# they are not the same, because we are trying to explain variation in Y 
# with multiple predictors, hot just one

# 8. Include the A variable into the previous model, coded as a factor. 
# Describe how it is coded as a contrast. Does it appear to be signiﬁcant?
full_model <- lm(Y ~ A + T + C + P, data = dat)
stargazer(full_model, type = "text")  

# 9. Fit a model which also includes interaction terms between A and the other 
# variables, and describe the resulting set of regression coeﬃcients. 
int1 <- lm(Y ~ A * T + C + P, data = dat)
stargazer(int1, type = "text")

int2 <- lm(Y ~ A * C + T + P, data = dat)
stargazer(int2, type = "text")

int3 <- lm(Y ~ A * P + T + C, data = dat)
stargazer(int3, type = "text")

## Exercise 3: Model Validation
# 1. Consider again the model in the mCPT object, and call par() and plot() 
# to plot all the diagnostic plots generated by plot(mCPT) in a single ﬁgure.
par(mfrow = c(2, 2))
plot(mCPT)
# 2. Look at the ﬁrst and second plots: is there any reason to think that the 
# cloud data violate the assumptions of the model?
# looks fine
