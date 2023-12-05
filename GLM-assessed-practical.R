library(tidyverse)
library(xtable)
library(ggplot2)
library(gridExtra)
library(patchwork) # to combine plots
library(stargazer)
library(kableExtra)
library(sandwich) # for robust Standard Errors
library(MASS) # for stepwise AIC and BIC selection
library(rsq)
library(effects)
library(marginaleffects)
# Get colors from the tableau palette
plotcolors <- ggthemes::tableau_color_pal()(10)
## Set Paths for tables and figures
root = "/Users/ts/Git/Practicals"
tab = "/Users/ts/Library/CloudStorage/Dropbox/Apps/Overleaf/GLM Practical/Tables"
fig = "/Users/ts/Library/CloudStorage/Dropbox/Apps/Overleaf/GLM Practical/Figures"
code = "/Users/ts/Library/CloudStorage/Dropbox/Apps/Overleaf/GLM Practical/Code"

setwd(code)
file.copy(from = "/Users/ts/Git/Practicals/GLM-assessed-practical.R",
          to = "/Users/ts/Library/CloudStorage/Dropbox/Apps/Overleaf/GLM Practical/Code",
          overwrite = T)
setwd(root)
if (getwd() != root) {
  setwd(root)
}
# Load data
data_raw <- read_csv("mortg.csv")

### EDA ###
# Cast as factors
data <- data_raw %>%
  mutate(
    approved = as.factor(approved),
    self = as.factor(self),
    single = as.factor(single),
    white = as.factor(white),
    mcs = factor(mcs, levels = 1:4),
    hir = 100 * hir,
    lvr = 100 * lvr,
    odir = 100 * odir
  )

attach(data)
# Summary stats functions
summary_stats <- function(x) {
  c(mean = mean(x, na.rm = T), 
    median = median(x, na.rm = T),
    sd = sd(x, na.rm = T),
    min = min(x, na.rm = T),
    max = max(x, na.rm = T))
}

# Split data
data_num <- data %>% dplyr::select(hir, odir, lvr, uria)
data_fact <- data_raw %>% dplyr::select(approved, mcs, self, single, white)

# Get sumstats
# Apply the function to each column
numerical_summary <- t(sapply(data_num, summary_stats))
factor_summary <- t(sapply(data_fact, summary_stats))
sumstats <- rbind(numerical_summary, factor_summary)
colnames(sumstats) <- c("Mean", "Median", "SD", "Min", "Max")

# Make table
sumtable <- xtable(sumstats, caption = "Summary Statistics", label = "sumstats")

#### MODEL BUILDING ####
# Fit baseline model
baseline <- glm(approved ~ ., data = data, family = binomial(link = "logit"))

# Check how to cast mcs by 
data_num <- data_raw %>% mutate(
  approved = as.factor(approved),
  self = as.factor(self),
  single = as.factor(single),
  white = as.factor(white),
)

# Refit full model usin mcs as numerical
baseline_num <- glm(approved ~ ., data = data_num, 
                    family = binomial(link = "logit"))

# stepwise AIC on baseline
step_aic <- stepAIC(baseline, direction = "backward")
# AIC deteriorates when we drop predictors

# stepwise BIC on baseline
step_bic <- step(baseline, direction = "backward", k = log(nrow(data)))
# BIC also deteriorates

# Interaction terms
# fit maximally interacted model
maximal <- glm(approved ~ self * ., family = binomial, data = data)

step_interact_aic <- stepAIC(maximal, direction = "backward")
step_interact_bic <- step(maximal, direction = "backward", k = log(nrow(data)))

bic <- glm(approved ~ self + hir + odir + lvr + white, data = data, 
           family = binomial)
final <- glm(approved ~ uria + hir + odir + lvr + mcs + single + white + 
                        self*odir + self*white + self*uria + self, 
                      family = binomial, data = data)

# Do LRT
LRT_selection <- anova(bic, final, maximal, test = "Chisq")
# export LRT
model_selection <- xtable(LRT_selection, caption = "Model Selection: LRT Results",
                          label = "LRT-select")

# make tables with p, AIC, BIC, RSQ_KL
modelmat_additive <- matrix(NA, 3, 4)
modelmat_interact <- matrix(NA, 3, 4)
modelmat_additive[1,1] <- length(coef(baseline))
modelmat_additive[2,1] <- length(coef(step_aic))
modelmat_additive[3,1] <- length(coef(step_bic))
modelmat_interact[1,1] <- length(coef(maximal))
modelmat_interact[2,1] <- length(coef(bic))
modelmat_interact[3,1] <- length(coef(final))


modelmat_additive[1,2] <- AIC(baseline)
modelmat_additive[2,2] <- AIC(step_aic)
modelmat_additive[3,2] <- AIC(step_bic)
modelmat_interact[1,2] <- AIC(maximal)
modelmat_interact[2,2] <- AIC(bic)
modelmat_interact[3,2] <- AIC(final)

modelmat_additive[1,3] <- BIC(baseline)
modelmat_additive[2,3] <- BIC(step_aic)
modelmat_additive[3,3] <- BIC(step_bic)
modelmat_interact[1,3] <- BIC(maximal)
modelmat_interact[2,3] <- BIC(bic)
modelmat_interact[3,3] <- BIC(final)

modelmat_additive[1,4] <- rsq.kl(baseline)
modelmat_additive[2,4] <- rsq.kl(step_aic)
modelmat_additive[3,4] <- rsq.kl(step_bic)
modelmat_interact[1,4] <- rsq.kl(maximal)
modelmat_interact[2,4] <- rsq.kl(bic)
modelmat_interact[3,4] <- rsq.kl(final)

rownames(modelmat_additive) <- c("Baseline", "stepwise AIC on baseline", 
                                 "stepwise BIC on baseline")
rownames(modelmat_interact) <- c("Maximal Interaction Model", 
                                 "stepwise BIC on interaction model",
                                 "Final Model")
colnames(modelmat_additive) <- c("p", "AIC", "BIC", "$R^2_{KL}$")
colnames(modelmat_interact) <- c("p", "AIC", "BIC", "$R^2_{KL}$")
model_tab_add <- xtable(modelmat_additive, 
                        caption = "Model Selection: Additive Models", 
                        label = "select-add",
                        align = c("r", rep("c", 4)), digits = 3)
model_tab_int <- xtable(modelmat_interact, 
                        caption = "Model Selection: Interaction Models", 
                        label = "selectiontable-int",
                        align = c("r", rep("c", 4)), digits = 3)
sanitize_latex <- function(x) {
  gsub("\\$", "\\\\\\$", x, fixed = T)
}


#### Diagnostics ####
rsq.kl(final)
# Compute Cook's distances
cooks_distances <- cooks.distance(final)
# Create df for plotting
cooks_df <- data.frame(Index = 1:length(cooks_distances), 
                       CooksDistance = cooks_distances)
# Define cutoff 
cutoff <- 8 / (nrow(data) - length(coef(final)))
# Add a new column to indicate points above the cutoff
cooks_df$Label <- ifelse(cooks_df$CooksDistance > cutoff, 
                         as.character(cooks_df$Index), "")

# plot
cooks <- ggplot(cooks_df, aes(x = Index, y = CooksDistance)) +
  geom_bar(stat = "identity", position = "identity", 
           aes(fill = CooksDistance > cutoff), width = 1) +
  scale_fill_manual(values = c(plotcolors[4], plotcolors[3])) +
  geom_hline(yintercept = cutoff, linetype = "dashed", color = plotcolors[2]) +
  geom_text(aes(label = Label), vjust = -0.5, check_overlap = T) + 
  theme_minimal() +
  labs(x = "Observation Index",
       y = "Cook's Distance") +
  theme(legend.position = "none")

# Pull points that have high cook's dist
# Extract indices where Cook's distance is greater than the cutoff
influential_indices <- cooks_df$Index[cooks_df$CooksDistance > cutoff]

# Subset the original data using these indices
influential_points <- data[influential_indices, ] %>% 
  mutate(Index = influential_indices) %>% 
  dplyr::select(Index, everything())
# Make table to print in appendix
infl <- xtable(influential_points, 
               caption = "Observations with High Cook's distance",
               label = "infl")

# We would hope to see roughly unit variance of the standardized residuals
var(rstandard(final))

#### Interpretation ####
model_summary <- broom::tidy(final)

# Compute all main and interaction effects
all_effects <- allEffects(final)

# Main Effect / Focal CIs
compute_focal_CI <- function(mod, var, alpha = 0.05) {
  # Extract VCOV and coefs
  coefs_var <- vcov(mod)
  coefs <- coef(mod)
  pe <- coef(final)[var]
  # Calc the z-value using alpha
  z <- qnorm(1 - alpha / 2)
  
  # Calc SE for the focal term
  se <- sqrt(coefs_var[var, var])
  ci_l <- exp(pe - z * se)
  ci_u <- exp(pe + z * se)
  ci_contains_1 <- ifelse((ci_l <= 1 & ci_u >= 1), "TRUE", "FALSE")
  
  return(c(pe = round(exp(pe), 3), 
           se = round(exp(se), 3),
           ci_lower = round(ci_l,3), 
           ci_upper = round(ci_u, 3),
           ci_cont_1 = ci_contains_1))
}

## Make table for focal predictors
hir_ci <- compute_focal_CI(final, "hir")
lvr_ci <- compute_focal_CI(final, "lvr")
mcs_2_ci <- compute_focal_CI(final, "mcs2")
mcs_3_ci <- compute_focal_CI(final, "mcs3")
mcs_4_ci <- compute_focal_CI(final, "mcs4")
single_ci <- compute_focal_CI(final, "single1")

focal_cis <- rbind(hir_ci, lvr_ci, mcs_2_ci, mcs_3_ci, mcs_4_ci, single_ci)
colnames(focal_cis) <- c("Point Estimate", "SE", "95% CI Lower", 
                         "95% CI Upper", "CI contains 1")
rownames(focal_cis) <- c("hir", "lvr", "mcs2", "mcs3" , "mcs4", "single")

focal_CI <- xtable(focal_cis, digits = 3, align = c("r", rep("c", 5)),
                   caption = "CIs for Predictors not included in Interaction 
                   Terms (Odds Ratio Scale)", label = "focal")


# Interaction Effect CIs
compute_interaction_CI <- function(mod, var1, var2 = "self1", alpha = 0.05) {
  # Extract VCOV and coefs
  coefs_var <- vcov(mod)
  coefs <- coef(mod)
  pe <- coefs[var1] + coefs[var2]
  
  # Calc the z-value using alpha
  z <- qnorm(1 - alpha / 2)
  
  # Calc SE for the interaction term
  se <- sqrt(coefs_var[var1, var1] + coefs_var[var2,var2] + 
               2 * coefs_var[var1,var2])
  ci_l <- exp(pe - z * se)
  ci_u <- exp(pe + z * se)
  ci_contains_1 <- ifelse((ci_l <= 1 & ci_u >= 1), "TRUE", "FALSE")
  
  return(c(pe = round(exp(pe), 3), 
           se = round(exp(se), 3),
           ci_lower = round(ci_l,3), 
           ci_upper = round(ci_u, 3),
           ci_cont_1 = ci_contains_1))
}

## Make table for self = 1
selfodir_ci <- unname(compute_interaction_CI(final, "odir", "odir:self1"))
selfwhite_ci <- unname(compute_interaction_CI(final, "white1", "white1:self1"))
selfuria_ci <- unname(compute_interaction_CI(final, "uria", "uria:self1"))

## Make table for self = 0
odir_ci <- compute_focal_CI(final, "odir")
white_ci <- compute_focal_CI(final, "white1")
uria_ci <- compute_focal_CI(final, "uria")

interact_cis <- rbind(odir_ci, white_ci, uria_ci,
                      selfodir_ci, selfwhite_ci, selfuria_ci)

# Set column and row names
colnames(interact_cis) <- c("Point Estimate", "SE", "95% CI Lower", 
                            "95% CI Upper", "CI contains 1")
rownames(interact_cis) <- c("odir:self0", "white1:self0", "uria:self0",
                            "odir:self1", "white1:self1", "uria:self1")


interact_CI <- xtable(interact_cis, digits = 3, align = c("r", rep("c", 5)),
                      caption = "CIs for Predictors included in Interaction 
                      Terms (Odds Ratio Scale)",
                      label = "CI-inter")
# make table of AME estimates and CIs on odds-ratio scale
AME_ors <- tidy(marginaleffects::avg_slopes(final, type = "link")) %>% 
  dplyr::select(-c(5:7)) %>% 
  mutate(
    estimate = exp(estimate),
    std.error = exp(std.error),
    cil = exp(conf.low),
    cih = exp(conf.high),
    CI_contains_1 = (cil <= 1 & cih >= 1)
  ) %>% dplyr::select( -c(conf.low, conf.high))


colnames(AME_ors) <- c("Term", "Contrast", "Est. Coef. (OR scale)", "SE", 
                       "95% CI Lower", "95% CI Upper", "CI contains 1")

resultstable <- xtable(AME_ors, 
                       caption = "Average Marginal Effects on Odds Ratio Scale",
                       label = "AME",
                       align = c("r", rep("c", 7)), 
                       include.rownames = F, digits = 3)

#### Dispersion ####
mu_hat <- predict(final, type = "response") 
y <- as.numeric(data$approved ) - 1
V_mu_hat <- mu_hat * (1 - mu_hat) # Variance function for binomial distribution

phi_hat <- 1 / (nrow(data) - length(coef(final)) ) * 
  sum((y - mu_hat)^2 / V_mu_hat)
disp_adj <- sqrt(phi_hat)

# Adjust for overdispersion
AME_adj <- tidy(marginaleffects::avg_slopes(final, type = "link")) %>% 
  dplyr::select(-c(5:7)) %>% 
  mutate(
    cil = exp(estimate - 1.96 * disp_adj * std.error),
    cih = exp(estimate + 1.96 * disp_adj * std.error),
    estimate = exp(estimate),
    std.error = exp(std.error),
    CI_contains_1 = (cil <= 1 & cih >= 1)
  ) %>% dplyr::select( -c(conf.low, conf.high))

colnames(AME_adj) <- c("Term", "Contrast", "Est. Coef. (OR scale)", "SE", 
                       "95% CI Lower", "95% CI Upper", "CI contains 1")
resultstable_adj <- xtable(AME_adj, 
                           caption = "Average Marginal Effects on Odds Ratio Scale, 
                           adjusted for Overdispersion",
                           label = "CI-adj",
                       align = c("r", rep("c", 7)), 
                       include.rownames = F, digits = 3)

#### Effect plots ####
plot_list <- lapply(names(all_effects), function(name) {
  # Shorten subplot titles
  plot(all_effects[[name]], main = name)
})

focal_effects <- plot_list[1:4]
interaction_effects <- plot_list[5:7]

# Combine plots (ugly presets)
effects <- do.call(grid.arrange, c(plot_list, ncol = 3))
# Manually for plot formatting
## Focal predictors (not included in interaction terms)
eff_plot_hir <- plot(predictorEffect("hir", final),
     axes=list(y=list(type="response"),
               x=list(rug=T)), main = "hir",
     lines=list(col=plotcolors[1]))

eff_plot_lvr <- plot(predictorEffect("lvr", final),
              axes=list(y=list(type="response"),
                        x=list(rug=T)), main = "lvr",
              lines=list(col=plotcolors[2]))

eff_plot_mcs <- plot(predictorEffect("mcs", final),
              axes=list(y=list(type="response"),
                        x=list(rug=T)), main = "mcs",
              lines=list(col=plotcolors[3]))

eff_plot_single <- plot(predictorEffect("single", final),
              axes=list(y=list(type="response"),
                        x=list(rug=T)), main = "single",
              lines=list(col=plotcolors[4]))
# Collect plots
focal_effect_plot_list <- lapply(ls(pattern = "^eff_plot_"), get)
# Arrange
focal_effect_plots <- do.call(grid.arrange, 
                              c(focal_effect_plot_list, ncol = 2))

## Interaction Terms
interaction_eff_plot_odir <- plot(predictorEffect("odir", final),
                     axes=list(y=list(type="response"),
                               x=list(rug=T)), main = "odir * self",
                     lines=list(col=plotcolors[5]))

interaction_eff_plot_white <- plot(predictorEffect("white", final),
                     axes=list(y=list(type="response"),
                               x=list(rug=T)), main = "white * self",
                     lines=list(col=plotcolors[8]))

interaction_eff_plot_uria <- plot(predictorEffect("uria", final),
                     axes=list(y=list(type="response"),
                               x=list(rug=T)), main = "uria * self",
                     lines=list(col=plotcolors[7]))


# Collect plots
interaction_effect_plot_list <- lapply(ls(pattern = "^interaction_eff_plot_"), 
                                       get)
# Arrange
interaction_effect_plots <- do.call(grid.arrange, 
                                    c(interaction_effect_plot_list, nrow = 1))
#### EDA plots ####
# plot colors
plotcolors <- ggthemes::tableau_color_pal()(10)
# Continuous predictors, bivariate
plot_hir <- ggplot(data, aes(x = approved, y = hir, fill = approved)) +
  geom_violin(trim = T) + # Add violin plot
  geom_boxplot(width = 0.1, fill = "white") + # Add boxplot inside
  scale_fill_manual(values = plotcolors[1:2]) +
  theme_minimal() +
  labs(x = "Approved", y = "Ratio of Housing Expenses to Income") +
  theme(legend.position = "none")

plot_odir <- ggplot(data, aes(x = approved, y = odir, fill = approved)) +
  geom_violin(trim = T) + # Add violin plot
  geom_boxplot(width = 0.1, fill = "white") + # Add boxplot inside
  scale_fill_manual(values = plotcolors[3:4]) +
  theme_minimal() +
  labs(x = "Approved", y = "Ratio of other Debt Payments to Income") +
  theme(legend.position = "none")

plot_lvr <- ggplot(data, aes(x = approved, y = lvr, fill = approved)) +
  geom_violin(trim = T) + # Add violin plot
  geom_boxplot(width = 0.1, fill = "white") + # Add boxplot inside
  scale_fill_manual(values = plotcolors[5:6]) +
  theme_minimal() +
  labs(x = "Approved", y = "LTV Ratio") +
  theme(legend.position = "none")

plot_uria <- ggplot(data, aes(x = approved, y = uria, fill = approved)) +
  geom_violin(trim = T) + # Add violin plot
  geom_boxplot(width = 0.1, fill = "white") + # Add boxplot inside
  scale_fill_manual(values = plotcolors[7:8]) +
  theme_minimal() +
  labs(x = "Approved", y = "Unempl. Rate in Appl. Industry") +
  theme(legend.position = "none")

boxplots <- (plot_hir | plot_odir | plot_lvr | plot_uria) + 
  plot_layout(guides = 'collect') &
  theme(legend.position = "bottom")

# Continuous predictors, trivariate, self-employed
plot_self_hir <- ggplot(data, aes(x = approved, y = hir, fill = self)) +
  geom_violin(trim = T) +
  scale_fill_manual(values = plotcolors[1:2]) +
  theme_minimal() +
  labs(x = "Approved", y = "Ratio of Housing Expenses to Income") +
  theme(legend.position = "bottom")

plot_self_odir <- ggplot(data, aes(x = approved, y = odir, fill = self)) +
  geom_violin(trim = T) +
  scale_fill_manual(values = plotcolors[1:2]) +
  theme_minimal() +
  labs(x = "Approved", y = "Ratio of other Debt Payments to Income") +
  theme(legend.position = "bottom")

plot_self_lvr <- ggplot(data, aes(x = approved, y = lvr, fill = self)) +
  geom_violin(trim = T) +
  scale_fill_manual(values = plotcolors[1:2]) +
  theme_minimal() +
  labs(x = "Approved", y = "LTV Ratio") +
  theme(legend.position = "bottom")

plot_self_uria <- ggplot(data, aes(x = approved, y = uria, fill = self)) +
  geom_violin(trim = T) +
  scale_fill_manual(values = plotcolors[1:2]) +
  theme_minimal() +
  labs(x = "Approved", y = "Unempl. Rate in Appl. Industry") +
  theme(legend.position = "bottom")

violinplots <- (plot_self_hir|plot_self_odir|plot_self_lvr|plot_self_uria) + 
  plot_layout(guides = 'collect') &
  theme(legend.position = "bottom")

# Factor Predictors (mcs, self, single, white)
plot_mcs <- ggplot(data, aes(x = approved, fill = mcs)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = plotcolors[1:4]) +
  theme_minimal() +
  labs(x = "Approved", y = "Percentage") + 
  theme(legend.position = "bottom")

plot_self <- ggplot(data, aes(x = approved, fill = self)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = plotcolors[5:6]) +
  theme_minimal() +
  labs(x = "Approved", y = "Percentage") + 
  theme(legend.position = "bottom")

plot_single <- ggplot(data, aes(x = approved, fill = single)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = plotcolors[7:8]) +
  theme_minimal() +
  labs(x = "Approved", y = "Percentage") + 
  theme(legend.position = "bottom")

plot_white <- ggplot(data, aes(x = approved, fill = white)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = plotcolors[9:10]) +
  theme_minimal() +
  labs(x = "Approved", y = "Percentage") + 
  theme(legend.position = "bottom")

mosaicplots <- (plot_mcs | plot_self | plot_single | plot_white)

## Use Bar prop plots instead
# function to get proportions
data_prop <- function(cat) {
  data %>%
    group_by(!!sym(cat), approved) %>%
    summarise(Count = n(), .groups = 'drop') %>%
    mutate(Fraction = Count / sum(Count))
}

prop_plot_mcs <- ggplot(data_prop('mcs'), aes(x = mcs, y = Fraction, 
                                              fill = as.factor(approved))) +
  geom_bar(stat = "identity", position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = plotcolors[1:2]) +
  labs(x = "MCS", y = "Percentage", fill = "Approved") +
  theme_minimal() +
  theme(legend.position = "bottom")

prop_plot_self <- ggplot(data_prop('self'), aes(x = self, y = Fraction, 
                                                fill = as.factor(approved))) +
  geom_bar(stat = "identity", position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = plotcolors[3:4]) +
  labs(x = "Self", y = "Percentage", fill = "Approved") +
  theme_minimal() +
  theme(legend.position = "bottom")

prop_plot_single <- ggplot(data_prop('single'), aes(x = single, y = Fraction, 
                                                  fill = as.factor(approved))) +
  geom_bar(stat = "identity", position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = plotcolors[5:6]) +
  labs(x = "Single", y = "Percentage", fill = "Approved") +
  theme_minimal() +
  theme(legend.position = "bottom")

prop_plot_white <- ggplot(data_prop('white'), aes(x = white, y = Fraction, 
                                                  fill = as.factor(approved))) +
  geom_bar(stat = "identity", position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = plotcolors[7:8]) +
  labs(x = "White", y = "Percentage", fill = "Approved") +
  theme_minimal() +
  theme(legend.position = "bottom")

prop_plots <- (prop_plot_mcs|prop_plot_self|prop_plot_single|prop_plot_white)

# Decile plots
plot_deciles <- function(var_name) {
  
  var <- data[[var_name]]
  
  # Calculate group-wise means
  breaks <- quantile(var, probs = seq(0, 1, by = 0.1), na.rm = T, 
                     names = F, type = 7)
  breaks[length(breaks)] <- max(var, na.rm = T)  # Ensure max  is included
  group_means <- tapply(as.numeric(data$approved) - 1, 
          findInterval(var, breaks, rightmost.closed = T), mean, na.rm = T)
  group_means <- na.omit(group_means)
  
  # Convert to data frame for ggplot
  plot_data <- data.frame(
    Deciles = factor(names(group_means), 
                     levels = as.character(1:length(group_means))),
    Mean = as.numeric(group_means)
  )
  
  # plot
  ggplot(plot_data, aes(x = Deciles, y = Mean, fill = factor(Deciles))) +
    geom_bar(stat = "identity", show.legend = F) +
    scale_fill_manual(values = plotcolors[1:10]) +
    theme_minimal() +
    theme(legend.position = "none") +
    ylim(0, 1) +
    labs(x = var_name, y = "Mean", )
}

decile_hir <- plot_deciles('hir')
decile_odir <- plot_deciles('odir')
decile_lvr <- plot_deciles('lvr')
decile_uria <- plot_deciles('uria')

# Stack the plots vertically
decile_plots <- decile_hir / decile_odir / decile_lvr / decile_uria

#### Export ####
# tables
setwd(tab)
print.xtable(sumtable, type = "latex", file = "sumstats.tex", 
             include.rownames = T, digits = 2, align = c("l", rep("c", 4)),
             caption.placement = "top", 
             floating = T, table.placement = "H")

print.xtable(model_selection, type = "latex", file = "selection.tex", 
             include.rownames = T, digits = 2, 
             caption.placement = "top", 
             floating = T, table.placement = "H")

print.xtable(model_tab_add, type = "latex", file = "models-add.tex", 
             include.rownames = T,
             caption.placement = "top",
             sanitize.text.function = sanitize_latex,
             table.placement = "H")

print.xtable(model_tab_int, type = "latex", file = "models-int.tex", 
             include.rownames = T,
             caption.placement = "top",
             sanitize.text.function = sanitize_latex,
             table.placement = "H")

print.xtable(focal_CI, type = "latex", file = "focal-ci.tex",
             include.rownames = T,
             digits = 3,
             caption.placement = "top",
             table.placement = "H")

print.xtable(interact_CI, type = "latex", file = "interaction-ci.tex",
             include.rownames = T,
             digits = 3,
             caption.placement = "top",
             table.placement = "H",
             hline.after = c(-1,0,3,6))

print.xtable(resultstable, type = "latex", file = "AME.tex",
             include.rownames = T,
             caption.placement = "top",
             table.placement = "H")

print.xtable(resultstable_adj, type = "latex", file = "AME-adj.tex",
             include.rownames = T,
             caption.placement = "top",
             table.placement = "H")

print.xtable(infl, type = "latex", file = "infl.tex",
             include.rownames = F,
             caption.placement = "top",
             table.placement = "H",
             size = "tiny")

stargazer(baseline, baseline_num, title="Estimation Results for Baseline Model", 
          type="latex", 
          label = "baselineresults",
          align=T, 
          out = "baseline.tex",
          column.labels=c("MCS as factor", "MCS as numerical"), 
          dep.var.caption="", 
          dep.var.labels.include = F, 
          no.space=T, 
          single.row=T, 
          header=F, 
          digits=3, notes = "SE in Parentheses")

stargazer(bic, maximal, final, title="Estimation Results for Interaction Models", 
          type="latex", 
          align=T, 
          out = "regtable.tex", 
          label = "regresults",
          column.labels=c("BIC Model", "Maximal Interation Model",  "Final Model"),
          dep.var.caption="", 
          dep.var.labels.include = F, 
          no.space=T, 
          single.row=T, 
          header=F, 
          digits=3, notes = "SE in Parentheses")
setwd(root)

# figures
setwd(fig)
ggsave(plot = boxplots, "boxplots.png")
ggsave(plot = mosaicplots, "mosaicplots.png")
ggsave(plot = prop_plots, "prop-plots.png")
ggsave(plot = violinplots, "violinplots.png")
ggsave(plot = decile_plots, "decileplots.png")
ggsave(plot = cooks, "cooksdist.png")
ggsave(plot = effects, "effects.png", dpi = 1000, height = 30, width = 30, units = "cm")
ggsave(plot = focal_effect_plots, "focal-effects.png", dpi = 1000, height = 30, width = 30, units = "cm")
ggsave(plot = interaction_effect_plots, "interaction-effects.png", dpi = 1000, height = 30, width = 30, units = "cm")
setwd(root)

