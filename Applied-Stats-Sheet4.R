# Problem 4 sheet 4
aids <- read.csv("http://www.stats.ox.ac.uk/~laws/SB1/data/aids.csv") 
head(aids)


aids$qrt <- as.factor(aids$qrt) plot(cases ~ date, data=aids)