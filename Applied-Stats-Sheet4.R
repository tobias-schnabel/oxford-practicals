### Sheet 4
#1a
pchisq((272.12 - 263.12), 3, lower.tail = F)

## Problem 2
disease <- c(77,19,47,48,16,31) 
healthy <- c(381,128,447,336,111,433) 
gender <- c('M','M','M','F','F','F') 
feed <- c('Bottle','Suppl','Breast','Bottle','Suppl','Breast') 
m <- disease + healthy 
y <- disease 
(X <- model.matrix(cbind(disease,healthy) ~ gender + feed))


# c
# Assuming you have the reduced model 'gender_model' and the full model 'full_model'
gender_model <- glm(cbind(disease, healthy) ~ gender, family = binomial, data = bf)
full_model <- glm(cbind(disease, healthy) ~ gender + feed, family = binomial, data = bf)

# Compare the models with an ANOVA to test for the effect of feed
anova_result <- anova(gender_model, full_model, test = "Chisq")
anova_result

# d
interaction_model <- glm(cbind(disease, healthy) ~ gender * feed, family = binomial, data = bf)

# Check the summary for residual deviance and degrees of freedom
summary_interaction_model <- summary(interaction_model)

# The residual deviance should be checked to confirm if it's zero or close to zero
residual_deviance <- summary_interaction_model$deviance
residual_deviance
summary_interaction_model$coefficients

# e
# IRLS - initialise using mu=y (>0) and formulae from Sheet 3, question 1 
mu <- y 
eta <- log(mu/(m-mu)) 
z <- eta 
W <- diag(c(mu*(m-mu)/m))
k <- 1 
dif <- 1 
beta <- solve(t(X)%*%W%*%X) %*% t(X)%*%W%*%z

while((k < 1000) && (dif>1e-05)) { 
  k <- k+1 
  betao <- beta

# update eta, mu, W and z eta <- X %*% beta

  mu <- m*exp(eta)/(1+exp(eta))

  W <- diag(c(mu*(m-mu)/m)) 
  z <- eta + m*(y-mu)/(mu*(m-mu)) 
  beta <- solve(t(X)%*%W%*%X) %*% t(X)%*%W%*%z

  dif <- max(abs(betao-beta))

}

# Fisher Info
eta <- X %*% beta

mu <- m*exp(eta)/(1+exp(eta))

W <- diag(c(mu*(m-mu)/m)) 
Iinv <- solve(t(X)%*%W%*%X)

# standard errors 
sqrt(diag(Iinv))

### Problem 3
pchisq((70.25 - 47.61), df = 5, lower.tail = FALSE)

pchisq((70.25 - 49.18), df = 2, lower.tail = FALSE)




### Problem 4
aids <- read.csv("http://www.stats.ox.ac.uk/~laws/SB1/data/aids.csv") 
head(aids)


aids$qrt <- as.factor(aids$qrt) 
plot(cases ~ date, data=aids)

# a
# log link: models the log of the expected value of the response variable as a 
# linear combination of predictors

# Square-root Link: models the square root of the expected value of the response
# variable as a linear function of predictors

# b
model_log <- glm(cases ~ ., family=poisson(link="log"), data=aids)
model_sqrt <- glm(cases ~ ., family=poisson(link="sqrt"), data=aids)
summary(model_log)
summary(model_sqrt)
# sqrt model gives lower AIC and residual deviance = better fit
# c fit restricted model
model_sqrt_rest <- glm(cases ~ date, family=poisson(link="log"), data=aids)
DP <- deviance(model_sqrt)
DR <- deviance(model_sqrt_rest)
LRT <- DR - DP
p <- model_sqrt$rank
r <- model_sqrt_rest$rank
1-pchisq(LRT, p-r) # reject Null

# date coeff. is highly stat. sig., indicates that for each unit increase in the 
# date, the expected number of AIDS cases increases by 
# approximately 3.34, holding other variables constant
# intercept does not have meaningful interpretation

# d
std_residuals <- rstandard(model_sqrt)
plot(std_residuals)
abline(h = c(-2, 2), col = "red")
# we have a substantial number of residuals outside the [-2,2] range, indicating
# potential misfit
chi_sq <- model_sqrt$deviance
df <- model_sqrt$df.residual
pchisq(chi_sq, df, lower.tail = FALSE)
# very low p-value (0), indicating bad fit too
plot(model_sqrt, which = c(4, 5))
# obs 29, 42, 44 are esp. high-influence
