### Sheet 4
#1a
pchisq((272.12 - 263.12), 3, lower.tail = F)

## Problem 2
# a
# Step 1: Calculate the Odds
bf <- data.frame(
  disease = c(77, 19, 47, 48, 16, 31),
  healthy = c(381, 128, 447, 336, 111, 433),
  gender = c('M', 'M', 'M', 'F', 'F', 'F'),
  feed = c('Bottle', 'Suppl', 'Breast', 'Bottle', 'Suppl', 'Breast')
)

# Calculate odds for each subgroup
bf$odds <- bf$disease / bf$healthy

# Step 2: Calculate Log-Odds Ratios for breast against bottle
lor_breast_bottle <- log(bf$odds[bf$feed == 'Breast'] / bf$odds[bf$feed == 'Bottle'])

# Step 3: Calculate the 95% CI for LOR
# Fit the model
model <- glm(cbind(disease, healthy) ~ gender + feed, family = binomial, data = bf)
summary_model <- summary(model)

# Extract the coefficients for breast and bottle feed
coef_breast <- summary_model$coefficients['feedBreast', ]
coef_bottle <- summary_model$coefficients['(Intercept)', ]  # Bottle is the reference level

# Calculate the standard error for LOR
se_lor <- sqrt(coef_breast['Std. Error']^2 + coef_bottle['Std. Error']^2)

# Calculate the 95% CI for the LOR
z_value <- 1.96
ci_lower <- lor_breast_bottle - z_value * se_lor
ci_upper <- lor_breast_bottle + z_value * se_lor

# Step 4: Test for difference in log-odds
z_stat <- lor_breast_bottle / se_lor
p_value <- 2 * (1 - pnorm(abs(z_stat)))

# Output the results
list(
  Odds = bf$odds,
  Log_Odds_Ratio = lor_breast_bottle,
  CI_Lower = ci_lower,
  CI_Upper = ci_upper,
  Z_Statistic = z_stat,
  P_Value = p_value
)

# b
# Fit the null model
null_model <- glm(cbind(disease, healthy) ~ 1, family = binomial, data = bf)

# Fit the full model (already fitted earlier)
full_model <- glm(cbind(disease, healthy) ~ gender + feed, family = binomial, data = bf)

# Conduct the likelihood ratio test
lrt_statistic <- -2 * (logLik(null_model) - logLik(full_model))
p_value <- pchisq(lrt_statistic, df = 5 - 1, lower.tail = FALSE)

# Output the test statistic and p-value
list(LRT_Statistic = lrt_statistic, P_Value = p_value)

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
# Create the design matrix
X <- model.matrix(~ gender + feed, data = bf)

# Response variable (number of disease cases)
y <- bf$disease

# Initial fit using glm to get starting values for mu
initial_fit <- glm(cbind(disease, healthy) ~ gender + feed, family = binomial, data = bf)

# Initial mu (predicted probabilities)
mu <- fitted(initial_fit)

# Linear predictor (logit of the probabilities)
eta <- log(mu / (1 - mu))

# Initialize the working response variable 'z'
z <- eta + (y - mu) / (mu * (1 - mu))

# Initialize the weight matrix
W <- diag(as.vector(mu * (1 - mu)))

# run IRLS
its <- 10
for (k in 1:its) {
  beta <- solve(t(X) %*% W %*% X) %*% t(X) %*% W %*% z
  eta <- X %*% beta
  mu <- exp(eta) / (1 + exp(eta))
  z <- eta + (y - mu) / (mu * (1 - mu))
  W <- diag(as.vector(mu * (1 - mu)))
}

# Compute standard errors
fisher_info <- t(X) %*% W %*% X
std_errors <- sqrt(diag(solve(fisher_info)))

### Problem 3
pchisq((70.25 - 47.61), df = 5, lower.tail = FALSE)

pchisq((70.25 - 49.18), df = 2, lower.tail = FALSE)




### Problem 4
aids <- read.csv("http://www.stats.ox.ac.uk/~laws/SB1/data/aids.csv") 
head(aids)


aids$qrt <- as.factor(aids$qrt) plot(cases ~ date, data=aids)