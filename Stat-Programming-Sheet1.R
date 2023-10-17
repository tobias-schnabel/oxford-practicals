#1
#Generate the following sequences and matrices

#(a) 1, 3, 5, 7, . . . , 21.
s1 = seq(from=1, to=21, by=2)

#(b) 50, 47, 44, . . . , 14, 11.
s2 = seq(from=50, to=11, by=-3)

#(c) 1, 2, 4, 8, . . . , 1024.
s3 = 2^(0:10)

#(d)
s4 = matrix(1:16, ncol = 4)

#2
#a
v1 = rep(1:4, each = 5)
#b
v2 = sample(v1)
#c
LETTERS[v2]

#3
#a
D = sample(c(-1, 1), 1e3, replace = TRUE)
#b
X = cumsum(D)
#c
plot(X, type="l")
#d
D2 = rbinom(25, 1, 0.5) * 2 - 1
X25 = cumsum(D)
#e
n = 25
num_realizations = 10e3

x <- 2 * rbinom(1e5, 25, 0.5) - 25

mean(x > 10)

#f
# Calculate the exact probability of X25 exceeding 10
exact_prob_exceed_10 = 1 - pbinom(10, 25, 0.5)


#4
#a
D = diag((1/(1:10)))

#b
M2 = matrix(-1, ncol=10, nrow=10)
diag(M2) = 4
U = M2
#c
col1_length = sqrt(sum(U[, 1]^2))
#d
U = sweep(U, 2, sqrt(rowSums(U^2)), FUN = '/')
# U = U / sqrt(sum(U[, 1]^2))
# stopifnot(all.equal(sqrt(sum(U[, 1]^2)), 1))
stopifnot(all.equal(sqrt(colSums(U ** 2)), rep(1, ncol(U))))
#e
X = U%*%D%*%t(U)
eigen(X)$values

#f
(1/(1:10)) * t(U)

#5
convertToBinary <- function(x, I) {
  # Initialize binary representation
  binary <- numeric(I)
  
  for (i in 1:I) {
    # Multiply x by 2
    y <- 2 * x
    
    if (y >= 1) {
      # Set b_i = 1 if y >= 1
      binary[i] <- 1
    } else {
      # Set b_i = 0 if y < 1
      binary[i] <- 0
    }
    
    # Update x based on the binary digit
    x <- y - binary[i]
    
    # Check termination conditions
    if (x == 0 || i == I) {
      break
    }
  }
  
  return(binary)
}

bin_0.3 = convertToBinary(0.3, 500)
bin_0.1_three_times = convertToBinary((0.1+0.1+0.1), 500)

print(which.max(bin_0.3 != bin_0.1_three_times))
