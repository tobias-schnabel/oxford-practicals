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

Sigma = cov(marks)

K = matrix(0,5,5)

K[1:3,1:3] = solve(Sigma[1:3,1:3])

K[3:5,3:5] = K[3:5,3:5] + solve(Sigma[3:5,3:5])

K[3,3] = K[3,3] - 1/Sigma[3,3]

Sigma_hat = solve(K)

round(Sigma_hat - S,10)

round(solve(Sigma_hat), 10)

# (c) Hence carry out a likelihood ratio test to see whether this model is 
# a good ﬁt to the data.

# We have 4 0-corrs so we  compare twice the diff in the log-likelihoods 
# with Chi2-4df dsit

tr <- function(x) sum(diag(x))

n <- nrow(marks)

fit = eval(-n*((log(det(Sigma)) - log(det(Sigma_hat))) - 
           tr(Sigma %*% (solve(Sigma) - solve(Sigma_hat)))))

1-pchisq(fit, 4)
