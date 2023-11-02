library(ggm)
data(marks)
head(marks, 8)
solve(cov(marks))

# (a) Manually ﬁnd the MLE for the covariance matrix Σ, under the model 
# from lectures in which ‘analysis’ and ‘statistics’ are independent 
# of ‘mechanics’ and ‘vectors’ conditional on ‘algebra’.

colnames(marks)
# [1] "mechanics"  "vectors"    "algebra"    "analysis"  
# [5] "statistics"

# so 4,5 indep of 1,2 given 3

sigma = cov(marks)
K <- solve(cov(marks))
K_hat <- matrix(0, 5, 5)
K_hat[1:3, 1:3] <- K[1:3, 1:3]
K_hat[3:5, 3:5] <- K_hat[3:5, 3:5] + K[3:5, 3:5]
K_hat[3, 3] <- K[3,3] - (1 / sigma[3, 3])

rownames(K_hat) <- rownames(K)
colnames(K_hat) <- colnames(K)
round(K_hat, 3)
round(K, 3)

