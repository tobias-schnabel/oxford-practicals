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

attach(data)
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
sumtable <- xtable(sumstats, caption = "Summary Statistics", label = "sumtable")

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

## Use Bar prop plots instead
# function to get proportions
data_prop <- function(cat) {
  data %>%
  group_by(!!sym(cat), approved) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  mutate(Fraction = Count / sum(Count))
}

prop_plot_mcs <- ggplot(data_prop('mcs'), aes(x = mcs, y = Fraction, fill = as.factor(approved))) +
  geom_bar(stat = "identity", position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = plotcolors[1:2]) +
  labs(x = "MCS", y = "Percentage", fill = "Approved") +
  theme_minimal() +
  theme(legend.position = "bottom")

prop_plot_self <- ggplot(data_prop('self'), aes(x = self, y = Fraction, fill = as.factor(approved))) +
  geom_bar(stat = "identity", position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = plotcolors[3:4]) +
  labs(x = "Self", y = "Percentage", fill = "Approved") +
  theme_minimal() +
  theme(legend.position = "bottom")

prop_plot_single <- ggplot(data_prop('single'), aes(x = single, y = Fraction, fill = as.factor(approved))) +
  geom_bar(stat = "identity", position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = plotcolors[5:6]) +
  labs(x = "Single", y = "Percentage", fill = "Approved") +
  theme_minimal() +
  theme(legend.position = "bottom")

prop_plot_white <- ggplot(data_prop('white'), aes(x = white, y = Fraction, fill = as.factor(approved))) +
  geom_bar(stat = "identity", position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = plotcolors[7:8]) +
  labs(x = "White", y = "Percentage", fill = "Approved") +
  theme_minimal() +
  theme(legend.position = "bottom")

prop_plots <- (prop_plot_mcs | prop_plot_self | prop_plot_single | prop_plot_white)

# Decile plots
plot_deciles <- function(var_name) {

  var <- data[[var_name]]
  
  # Calculate group-wise means, ensuring the max value is included in the intervals
  breaks <- quantile(var, probs = seq(0, 1, by = 0.1), na.rm = TRUE, names = FALSE, type = 7)
  breaks[length(breaks)] <- max(var, na.rm = TRUE)  # Ensure the maximum value is included
  group_means <- tapply(as.numeric(data$approved) - 1, findInterval(var, breaks, rightmost.closed = TRUE), mean, na.rm = TRUE)
  group_means <- na.omit(group_means)
  
  # Convert to data frame for ggplot
  plot_data <- data.frame(
    Deciles = factor(names(group_means), levels = as.character(1:length(group_means))),
    Mean = as.numeric(group_means)
  )
  
  # plot
  ggplot(plot_data, aes(x = Deciles, y = Mean, fill = factor(Deciles))) +
    geom_bar(stat = "identity", show.legend = FALSE) +
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

#### MODEL BUILDING ####
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

# Compare models using LRT
anova_cast <- anova(baseline_num, baseline, test="Chisq")
mcs_table <- xtable(anova_cast, caption = "Model Selection: MCS Data Type", label = "lrt-mcs")
# stepwise AIC
step_aic <- stepAIC(baseline, direction = "backward")
# AIC deteriorates when we drop predictors

# BIC
step_bic <- step(baseline, direction = "backward", k = log(nrow(data)))
# BIC also deteriorates

# Interaction terms
# fit maximally interacted model
maximal <- glm(approved ~ self * ., family = binomial, data = data)

step_interact <- stepAIC(maximal, direction = "backward")
final <- glm(approved ~ uria + hir + odir + lvr + mcs + single + white + 
                        self*odir + self*white + self*uria + self, 
                      family = binomial, data = data)

LRT_selection <- anova(final, maximal, test = "Chisq")
model_selection <- xtable(LRT_selection, caption = "Model Selection: LRT Results", label = "lrt-select")
LRT_coefficients <- anova(final, test = "Chisq")
model_coefs <- xtable(LRT_selection, caption = "Estimated Coefficients: LRT Results", label = "lrt-coefs")



#### Diagnostics ####
rsq.kl(final)


#### Export ####
# tables
setwd(tab)
print.xtable(sumtable, type = "latex", file = "sumstats.tex", 
             include.rownames = TRUE, digits = 2, align = c("l", rep("c", 4)),
             caption.placement = "top", 
             floating = T, table.placement = "H")

print.xtable(model_selection, type = "latex", file = "selection.tex", 
             include.rownames = TRUE, digits = 2, 
             caption.placement = "top", 
             floating = T, table.placement = "H")

print.xtable(model_coefs, type = "latex", file = "coefs.tex", 
             include.rownames = TRUE, digits = 2,
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

stargazer(final, title="Estimation Results for Final Model", 
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
ggsave(plot = prop_plots, "prop-plots.png")
ggsave(plot = violinplots, "violinplots.png")
ggsave(plot = decile_plots, "decileplots.png")
setwd(root)

