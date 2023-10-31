library(ggm)
data(marks)
head(marks, 8)
solve(cov(marks))

# (a) Manually ﬁnd the MLE for the covariance matrix Σ, under the model 
# from lectures in which ‘analysis’ and ‘statistics’ are independent 
# of ‘mechanics’ and ‘vectors’ conditional on ‘algebra’.

