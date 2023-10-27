# non-assessed practical week 3 MT23

# The data set we are considering today describes a cloud seeding experiment aimed 
# at increasing rainfalls, taken from Cook and Weisberg’s “Residuals and Inference 
# in Regression” book. It used silver iodide as a catalyst to induce rain, and 
# targeted an area of 3000 square miles north-east of Coral Gable, California 
# for 24 days in the summer of 1975. The following variables were recorded:
library(tidyverse, stargazer)
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
