##
## Q1
##

test_that("can calculate moving average for k=3", {
    x <- 1:10
    expected_result <- numeric(10)
    expected_result[1] <- 1.5
    expected_result[2:8] <- 2:8
    expected_result[10] <- 9.5
    expect_equal(
        calculate_moving_average_for_k(x, 3),
        expected_result
    )
})

library("testthat")
calculate_moving_average_for_k <- function(x, k) {
    n <- length(x)
    sapply(1:n, function(j) {
        i1 <- max(1, j - (k - 1) / 2)
        i2 <- min(n, j + (k - 1) / 2)
        (1 / (i2 - i1 + 1)) * sum(x[i1:i2])
    })
}

test_that("can calculate moving average for k=3", {
    x <- 1:10
    expected_result <- numeric(10)
    expected_result[1] <- 1.5
    expected_result[2:9] <- 2:9
    expected_result[10] <- 9.5
    expect_equal(
        calculate_moving_average_for_k(x, 3),
        expected_result
    )
})

calculate_moving_averages <- function(x, k_vec) {
    sapply(k_vec, function(k) {
        calculate_moving_average_for_k(x, k)
    })
}

test_that("can calculate moving averages for k=1, k=3", {
    x <- 1:10
    expected_result <- matrix(nrow = 10, ncol = 2)
    k_vec <- c(1, 3)
    expected_result[, 1] <- 1:10
    expected_result[, 2] <- c(1.5, 2:9, 9.5)
    expect_equal(
        calculate_moving_averages(x, k_vec),
        expected_result
    )
})


##
## Q2
##
n1 <- 200
n2 <- 100
nWeights <- 50
ref_allele_matrix <- matrix(sample(c(0, 1), n1 * n2, replace = TRUE), nrow = n1)
weight_matrix <- matrix(rnorm(n1 * n2, mean = 1, sd = 0.1), nrow = n1)
Gamma_Weights_States <- runif(nWeights)
row_update <- 1

initial_function <- function(
    ref_allele_matrix,
    weight_matrix,
    Gamma_Weights_States,
    row_update
){
    x1_sums <- colSums(
        weight_matrix[-row_update,]*ref_allele_matrix[-row_update,]
    )
    x1_matrix <- replicate(length(Gamma_Weights_States), x1_sums)
    x1_matrix <- t(x1_matrix)
    x2 <- outer(Gamma_Weights_States, ref_allele_matrix[row_update,], FUN ='*')
    B <- x1_matrix + x2
    S <- colSums(weight_matrix[-row_update,])
    A <- outer(S,Gamma_Weights_States, FUN='+')
    allele_frequencies <- t(B)/A
    allele_frequencies
}

new_function <- function(
    ref_allele_matrix,
    weight_matrix,
    Gamma_Weights_States,
    row_update
){
    x1_sums <- colSums(
        weight_matrix[-row_update,]*ref_allele_matrix[-row_update,]
    )
    x1_matrix <- replicate(length(Gamma_Weights_States), x1_sums)
    x1_matrix <- t(x1_matrix)
    x2 <- outer(Gamma_Weights_States, ref_allele_matrix[row_update,], FUN ='*')
    B <- x1_matrix + x2
    S <- colSums(weight_matrix[-row_update,])
    A <- outer(S,Gamma_Weights_States, FUN='+')
    allele_frequencies <- t(B)/A
    allele_frequencies
}

library("testthat")
my_test <- function() {
    n1 <- 200
    n2 <- 100
    nWeights <- 50
    ref_allele_matrix <- matrix(
        sample(c(0, 1), n1 * n2, replace = TRUE), 
        nrow = n1
    )
    weight_matrix <- matrix(rnorm(n1 * n2, mean = 1, sd = 0.1), nrow = n1)
    Gamma_Weights_States <- runif(nWeights)
    row_update <- 1
    expect_equal(
        initial_function(
            ref_allele_matrix,
            weight_matrix,
            Gamma_Weights_States,
            row_update
        ),
        new_function(
            ref_allele_matrix,
            weight_matrix,
            Gamma_Weights_States,
            row_update
        )
    )
}
    
my_test()


library("microbenchmark")
my_benchmark <- function() {
    microbenchmark(
        initial_function(
            ref_allele_matrix,
            weight_matrix,
            Gamma_Weights_States,
            row_update
        ),
        new_function(
            ref_allele_matrix,
            weight_matrix,
            Gamma_Weights_States,
            row_update
        ),
        times = 1000L
    )
}
my_benchmark()

library("profvis")
profvis({
    for(i in 1:1000) {
        x1_sums <- colSums(
            weight_matrix[-row_update,]*ref_allele_matrix[-row_update,]
        )
        x1_matrix <- replicate(length(Gamma_Weights_States), x1_sums)
        x1_matrix <- t(x1_matrix)
        x2 <- outer(
            Gamma_Weights_States, ref_allele_matrix[row_update,], 
            FUN ='*'
        )
        B <- x1_matrix + x2
        S <- colSums(weight_matrix[-row_update,])
        A <- outer(S,Gamma_Weights_States, FUN='+')
        allele_frequencies <- t(B)/A
        allele_frequencies
    }
})

colSums(weight_matrix[-row_update,]*ref_allele_matrix[-row_update,])

microbenchmark(
    colSums(weight_matrix[-row_update,]*ref_allele_matrix[-row_update,]),
    colSums(
        weight_matrix * ref_allele_matrix
    ) - (
        weight_matrix[row_update, ] * ref_allele_matrix[row_update, ]
    ),
    times = 1000L
)

new_function <- function(
    ref_allele_matrix,
    weight_matrix,
    Gamma_Weights_States,
    row_update
){
    x1_sums <- colSums(
        weight_matrix * ref_allele_matrix
    ) - (
        weight_matrix[row_update, ] * ref_allele_matrix[row_update, ]
    )
    x1_matrix <- replicate(length(Gamma_Weights_States), x1_sums)
    x1_matrix <- t(x1_matrix)
    x2 <- outer(Gamma_Weights_States, ref_allele_matrix[row_update,], FUN ='*')
    B <- x1_matrix + x2
    S <- colSums(weight_matrix[-row_update,])
    A <- outer(S,Gamma_Weights_States, FUN='+')
    allele_frequencies <- t(B)/A
    allele_frequencies
}

my_test()
my_benchmark()

x1_matrix <- replicate(length(Gamma_Weights_States), x1_sums)
x1_matrix <- t(x1_matrix)

## add to global environment
x1_sums <- colSums(
    weight_matrix * ref_allele_matrix
) - (
    weight_matrix[row_update, ] * ref_allele_matrix[row_update, ]
)

## where f1 is the original version and f2 and f3 are proposed alternates
## here we use functions to keep things clear
f1 <- function() {
    x1_matrix <- replicate(length(Gamma_Weights_States), x1_sums)
    x1_matrix <- t(x1_matrix)
    return(NULL)
}
f2 <- function() {
    matrix(
        rep(x1_sums, each = length(Gamma_Weights_States)), 
        ncol = length(x1_sums)
    )
    return(NULL)
}
f3 <- function() {
    x1_matrix <- matrix(
        0, 
        nrow = length(Gamma_Weights_States), 
        ncol = length(x1_sums)
    )
    for(i in 1:length(x1_sums)) {
        x1_matrix[, i] <- x1_sums[i]
    }
    return(NULL)
}

microbenchmark(
    f1(),
    f2(),
    f3(),
    times = 1000L
)

new_function <- function(
    ref_allele_matrix,
    weight_matrix,
    Gamma_Weights_States,
    row_update
){
    x1_sums <- colSums(
        weight_matrix * ref_allele_matrix
    ) - (
        weight_matrix[row_update, ] * ref_allele_matrix[row_update, ]
    )
    x1_matrix <- matrix(
        0, 
        nrow = length(Gamma_Weights_States), 
        ncol = length(x1_sums)
    )
    for(i in 1:length(x1_sums)) {
        x1_matrix[, i] <- x1_sums[i]
    }
    x2 <- outer(Gamma_Weights_States, ref_allele_matrix[row_update,], FUN ='*')
    B <- x1_matrix + x2
    S <- colSums(weight_matrix[-row_update,])
    A <- outer(S,Gamma_Weights_States, FUN='+')
    allele_frequencies <- t(B)/A
    allele_frequencies
}

my_test()
my_benchmark()

colSums(weight_matrix[-row_update,])

microbenchmark(
    colSums(weight_matrix[-row_update,]),
    colSums(weight_matrix) - weight_matrix[row_update, ],
    times = 1000L
)

new_function <- function(
    ref_allele_matrix,
    weight_matrix,
    Gamma_Weights_States,
    row_update
){
    x1_sums <- colSums(
        weight_matrix * ref_allele_matrix
    ) - (
        weight_matrix[row_update, ] * ref_allele_matrix[row_update, ]
    )
    x1_matrix <- matrix(
        0, 
        nrow = length(Gamma_Weights_States), 
        ncol = length(x1_sums)
    )
    for(i in 1:length(x1_sums)) {
        x1_matrix[, i] <- x1_sums[i]
    }
    x2 <- outer(Gamma_Weights_States, ref_allele_matrix[row_update,], FUN ='*')
    B <- x1_matrix + x2
    S <- colSums(weight_matrix) - weight_matrix[row_update, ]
    A <- outer(S,Gamma_Weights_States, FUN='+')
    allele_frequencies <- t(B)/A
    allele_frequencies
}

my_test()
my_benchmark()

my_benchmark()

