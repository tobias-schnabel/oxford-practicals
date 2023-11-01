## 2. Three separate samples, one from each of four different suspensions of 
#bacteria A, B, C and D (the 4 ‘treatments’) were prepared. 
#Technician I examined under a microscope one sample from each suspension in 
#random order. Similarly the other samples were tested by Technicians II and III 
#(the 3 ‘blocks’).
# 
# The recorded number of organisms from each sample are summarised in the 
#following table. Interest principally centres on how suspensions A, B, C and D 
#affect the recorded number of organisms.
# 
# (a) What is this type of experimental design called? Explain how to set up the data
# for analysis using the lm() function in R, treating the block and treatment label
# for each measurement as a categorical variable.
# Copy Data from sheet

# Blocked design / RCBD 
data <- data.frame(
  Organisms = c(67,84,77,60,71,77,74,70,54,67,65,56),
  Suspension = factor(rep(LETTERS[1:4], times=3)),
  Technician = factor(rep(c('I', 'II', 'III'), each=4))
)
# Linear model
model <- lm(Organisms ~ Suspension + Technician, data=data)

# (b) Use R to construct the analysis of variance table to test the 
#hypothesis that there is no difference between the suspensions. 
#Perform the test and state your conclusions.
anova(model)

# F-test for Suspension does not reject, insufficient evidence to reject H0

# (c) In (ii) you were asked to do a two-way analysis of variance. 
#Suppose the scientists had not recorded which technician made which count – 
#so the data are now:
data_new <- subset(data, select = 1:2)
# Would you reach the same conclusion if you did a one-way analysis of variance? 
#(i.e. ignoring the technician indicators).
model_oneway <- lm(Organisms ~ Suspension, data=data_new)

anova(model_oneway)
# yes, same conclusion

## 4
# 4. In a study investigating a new method of measuring body composition Mazess, 
# R.B., Peppler, W.W. and Gibbons, M. (1984) gave the body fat percentage, age 
# and sex for 18 normal adults aged between 23 and 61 years.
data <- read.table("http://www.stats.ox.ac.uk/~laws/SB1/data/bodyfat.txt", header=TRUE)
data$Sex <- as.factor(data$Sex)

# Write down a normal linear model where the response variable is 
# %fat and the explanatory variables are ‘Age’ and ‘Sex’. 
# Your model show allow the yearly change in %fat to differ for women and men. 
# Give full mathematical details.
model <- lm(Fat ~ Age * Sex, data=data)
summary(model)

#Does (a) body fat percentage increase as a function of age for women,
# no, insufficient evidence to reject H0 (Women are baseline)

#does (b) the yearly change in body fat percentage differ for men and women?
# no, insufficient evidence to reject H0 (Women are baseline)

##3

#a 
1 - pf(34.84, 1, 17)

#b
1 - pt(2.421, 17)

#c
2 * (1 - pt(abs(-0.96), 17))

#d
1 - pf(1.284792, 5, 17)

#check
a <- read.table("http://www.stats.ox.ac.uk/~laws/SB1/data/pearce.apple.txt", header = TRUE, stringsAsFactors = TRUE)
a$treatment <- relevel(a$treatment, ref = "S")

mod_intercept <- lm(yield ~ 1, data=a)
mod_history <- lm(yield ~ history, data=a)
mod_full <- lm(yield ~ history + treatment, data=a)
anova_intercept_history <- anova(mod_intercept, mod_history)
anova_history_full <- anova(mod_history, mod_full)

print(anova_intercept_history)
print(anova_history_full)

