library(tidyverse)
library(xtable)
library(ggplot2)
library(gridExtra)
library(patchwork) # to combine plots
library(stargazer)
library(kableExtra)
library(sandwich) # for robust Standard Errors
library(MASS) # for stepwise AIC and BIC selection
library(ggfortify)
library(rsq)

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
    mcs = factor(mcs, levels = 1:4)
  )
# Summary stats functions
summary_stats <- function(x) {
  c(mean = mean(x, na.rm = TRUE), 
    median = median(x, na.rm = TRUE),
    sd = sd(x, na.rm = TRUE),
    min = min(x, na.rm = TRUE),
    max = max(x, na.rm = TRUE))
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
sumtable <- xtable(sumstats, caption = "Summary Statistics", label = "tab1")

## EDA plots
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

violinplots <- (plot_self_hir | plot_self_odir | plot_self_lvr | plot_self_uria) + 
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

plot_lowdebt <- ggplot(data, aes(x = low_debt, fill = approved)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = plotcolors[9:10]) +
  theme_minimal() +
  labs(x = "Approved", y = "Percentage") + 
  theme(legend.position = "bottom")

# Fit full model
baseline <- glm(approved ~ ., data = data, family = binomial(link = "logit"))

# Check how to cast mcs
data_num <- data_raw %>% mutate(
  approved = as.factor(approved),
  self = as.factor(self),
  single = as.factor(single),
  white = as.factor(white),
)

# Refit full model
baseline_num <- glm(approved ~ ., data = data_num, family = binomial(link = "logit"))

# Diagnostic plots
baseline_fact_1 <- autoplot(baseline, which = 1)
baseline_fact_2 <- autoplot(baseline, which = 2)
baseline_fact_3 <- autoplot(baseline, which = 5)
baseline_fact_4 <- autoplot(baseline, which = 6)

baseline_num_1 <- autoplot(baseline_num, which = 1)
baseline_num_2 <- autoplot(baseline_num, which = 2)
baseline_num_3 <- autoplot(baseline_num, which = 5)
baseline_num_4 <- autoplot(baseline_num, which = 6)

# Arrange side by side
diag_plot_baseline <- baseline_fact_1 + baseline_num_1 + baseline_fact_2 + 
  baseline_num_2 +
  baseline_fact_3 + baseline_num_3 + baseline_fact_4 + baseline_num_4 +
  plot_layout(ncol = 2, nrow = 4)

# stepwise AIC
step_aic <- stepAIC(baseline, direction = "backward")
# AIC deteriorates when we drop predictors

# BIC
step_bic <- step(baseline, direction = "backward", k = log(nrow(data)))
# BIC also deteriorates

# Interaction terms
# Interaction between 'self' and 'white', including all other predictors
model_self_white <- glm(approved ~ self * white + hir + odir + lvr + mcs + 
                          single + uria, family = binomial, data = data)

# Interaction between 'self' and 'single', including all other predictors
model_self_single <- glm(approved ~ self * single + hir + odir + lvr + mcs 
                         + white + uria, family = binomial, data = data)

# Interaction between 'self' and 'lvr', including all other predictors
model_self_lvr <- glm(approved ~ self * lvr + hir + odir + mcs + single + white 
                      + uria, family = binomial, data = data)

# Interaction between 'self' and 'odir', including all other predictors
model_self_odir <- glm(approved ~ self * odir + hir + lvr + mcs + single + 
                         white + uria, family = binomial, data = data)

# Interaction between 'self' and 'hir', including all other predictors
model_self_hir <- glm(approved ~ self * hir + odir + lvr + mcs + single + white +
                        uria, family = binomial, data = data)

# Interaction between 'self' and 'hir', including all other predictors
model_self_hir <- glm(approved ~ self * uria + odir + lvr + mcs + single + white +
                        hir, family = binomial, data = data)

# Interaction between 'self' and 'hir', including all other predictors
model_self_hir <- glm(approved ~ self * mcs + odir + lvr + uria + single + white +
                        hir, family = binomial, data = data)

# All possible interactions on self
model_self_all <- glm(approved ~ self * ., family = binomial, data = data)

model_list <- mget(grep("^model_self_*", ls(), value = TRUE))

#compare ICs:
aic <- round(sapply(model_list, AIC), 2)
bic <- round(sapply(model_list, BIC), 2)
which(aic == min(aic))
which(bic == min(bic))

step_interact <- step(model_self_all, direction = "both")
model_interact <- glm(approved ~ self + hir + odir + lvr + mcs + single + white + 
                        self*odir + self*white + self*uria + uria, 
                      family = binomial, data = data)

anova(model_self_odir, model_interact, test = "Chisq")
anova(model_interact, test = "Chisq")

# Diagnostic plots
model_self_odir_1 <- autoplot(model_self_odir, which = 1)
model_self_odir_2 <- autoplot(model_self_odir, which = 2)
model_self_odir_3 <- autoplot(model_self_odir, which = 5)
model_self_odir_4 <- autoplot(model_self_odir, which = 6)

model_interact_1 <- autoplot(model_interact, which = 1)
model_interact_2 <- autoplot(model_interact, which = 2)
model_interact_3 <- autoplot(model_interact, which = 5)
model_interact_4 <- autoplot(model_interact, which = 6)

# Arrange side by side
diag_plot_interaction <- model_self_odir_1 + model_interact_1 + model_self_odir_2 + 
  model_interact_2 +
  model_self_odir_3 + model_interact_3 + model_self_odir_4 + model_interact_4 +
  plot_layout(ncol = 2, nrow = 4)

### Experiments
data_new <- data %>%
  mutate(low_debt = if_else(odir < 0.2, 1, 0)) %>% 
  dplyr::select(!odir)


baseline_new <- glm(approved ~ ., data = data_new, family = binomial(link = "logit"))

### Export ###
# tables
setwd(tab)
print.xtable(sumtable, type = "latex", file = "sumstats.tex", 
             include.rownames = TRUE, digits = 2, align = c("l", rep("c", 4)),
             caption.placement = "top", 
             floating = T, table.placement = "H")

stargazer(baseline, baseline_num, title="Estimation Results for Baseline Model", 
          type="latex", 
          align=TRUE, 
          out = "baseline.tex",
          column.labels=c("MCS as factor", "MCS as numerical"), 
          dep.var.caption="", 
          dep.var.labels.include = FALSE, 
          no.space=TRUE, 
          single.row=TRUE, 
          header=FALSE, 
          digits=3, notes = "SE in Parentheses")

stargazer(model_self_odir, model_interact, title="Estimation Results for Interaction Models", 
          type="latex", 
          align=TRUE, 
          out = "regtable.tex",
          column.labels=c("self * odir", "self * odir, self * white, self * uria"), 
          dep.var.caption="", 
          dep.var.labels.include = FALSE, 
          no.space=TRUE, 
          single.row=TRUE, 
          header=FALSE, 
          digits=3, notes = "SE in Parentheses")
setwd(root)

# figures
setwd(fig)
ggsave(plot = boxplots, "boxplots.png")
ggsave(plot = mosaicplots, "mosaicplots.png")
ggsave(plot = violinplots, "violinplots.png")
setwd(root)



