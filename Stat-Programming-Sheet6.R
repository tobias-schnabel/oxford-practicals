##
## Q1
##
library("parallel")


fork_results <- unlist(mclapply(1:100, mc.cores = 2, function(x) {
    mean(rnorm(10000))
}))

cl <- makeCluster(2)
socket_results <- parSapply(
    cl = cl, X = 1:100, FUN = function(x) {
        mean(rnorm(10000))
    }
)
stopCluster(cl)

## fork
fork_results <- unlist(mclapply(mtcars, mc.cores = 2, mean))

## sockets
cl <- makeCluster(2)
socket_results <- parSapply(cl = cl, X = mtcars, FUN = mean)
stopCluster(cl)




##
## Q2
##

autompg <- read.csv("autompg_clean.csv", header = TRUE)
training_samples <- sample(
    1:nrow(autompg), round(nrow(autompg) * 0.80), replace = FALSE
)
train <- autompg[training_samples , ]
test <- autompg[-training_samples, ]


one_linear_regression <- function() {
    train_bootstrapped <- train[
        sample(1:nrow(train), replace = TRUE), 
        ]
    lm(
        mpg ~ cylinders + displacement + horsepower + weight + 
            acceleration + model.year + origin, 
        data = train_bootstrapped
    )
}
fork_results <- mclapply(1:100, mc.cores = 2, function(x) {
    one_linear_regression()
})

## sockets
cl <- makeCluster(2)
clusterExport(cl = cl, "one_linear_regression")
clusterExport(cl = cl, "train")
socket_results <- parLapply(cl = cl, X = 1:100, fun = function(x) {
    one_linear_regression()
})
stopCluster(cl)


bagged_prediction <- rowMeans(
    sapply(fork_results, function(x) {
        predict(x, test)
    })
)

cor(bagged_prediction, test[, "mpg"])

normal_model <- lm(
    mpg ~ cylinders + displacement + horsepower + weight + 
        acceleration + model.year + origin, 
    data = train
)

normal_prediction <- predict(normal_model, test)

cor(normal_prediction, test[, "mpg"])

## marginal difference (4th digit)







##
## Q3
##
f <- function(M = 10, N = 100, T = 200) { 
    v <- array(NA, T)
    for(i in 1:T) {
        m <- matrix(runif(M * M), ncol = M) * N 
        v[i] <- prod(colSums(m))
    }
    v
}

f <- function(M = 10, N = 100, T = 200) { 
    v <- NULL
    for(i in 1:T) {
        m <- matrix(runif(M * M), ncol = M) * N 
        v <- c(v, prod(colSums(m)))
    }
    v
}



##
## Q4
##

sample_with_replacement <- function(x) {
    n <- length(x)
    x[ceiling(runif(n, 0, n))]
}

## for equal log spacing
ns <- 10 ** seq(5, 7, length.out = 10)

## for equal normal spacing
ns <- seq(10 ** 5, 10 ** 7, length.out = 10)

nReps <- 5
results <- t(sapply(ns, function(n) {
    x <- runif(n)
    return(
        c(
            n,
            system.time({
                for(i in 1:nReps) {
                    y <- sample_with_replacement(x)
                }
            })["elapsed"] / nReps
        )
    )
}))
plot(
    x = results[, 1], 
    y = results[, 2], 
    xlab = "n", 
    ylab = "Time elapsed (s)", 
    type = "b", 
    col = "blue", 
    lwd = 2
)
## looks linear 

bootstrap_mean_test <- function(x, y, B) {
    n <- length(x)
    m <- length(y)
    x_bar <- mean(x)
    x_sigma <- sd(x)
    y_bar <- mean(y)
    y_sigma <- sd(y)
    t <- (x_bar - y_bar) / sqrt(
        x_sigma ** 2 / n +
        y_sigma ** 2 / m
    )
    z_bar <- mean(c(x, y))
    x_1 <- x - x_bar + z_bar
    y_1 <- y - y_bar + z_bar
    ind <- 0
    for(i in 1:B) {
        x_star <- x_1[ceiling(runif(n, 0, n))]
        y_star <- y_1[ceiling(runif(m, 0, m))]
        stat <- (mean(x_star) - mean(y_star)) / sqrt(
            sd(x_star) ** 2 / n +
            sd(y_star) ** 2 / m
            )
        if (abs(stat) > abs(t)) {
            ind <- ind + 1
        }
    }
    p <- ind / B
    return(p)
}

## we can quickly verify this makes sense on some distributions

## expect to be the same
bootstrap_mean_test(runif(100), runif(100), B = 100)
bootstrap_mean_test(rnorm(100), rnorm(100), B = 100)

## expect to have a low p-value
bootstrap_mean_test(runif(100) + 10, runif(100), B = 100)
bootstrap_mean_test(runif(100), runif(100) + 10, B = 100)
bootstrap_mean_test(runif(100), runif(100) - 10, B = 100)
bootstrap_mean_test(runif(100) - 10, runif(100), B = 100)


## there are many many ways to do this
## below is one such way
## it's a bit tricky as the proposed is O(B(m + n))
## it's easy enough to check linear in B or (m+n)
## but we should also check it's not linear in JUST m or n
## hence the more advanced code below

time_one_run <- function(xn, yn, Bn) {
    nReps <- 30
    x <- runif(xn)
    y <- runif(yn)
    B <- Bn
    a <- 0
    system.time({for(i in 1:nReps) {
        bootstrap_mean_test(x, y, B)
    }})["elapsed"] / nReps
}


check_complexity <- function(x, y, B, xplotval, xlab) {
    results <- mapply(
        time_one_run, 
        xn = x,
        yn = y,
        Bn = B
    )
    plot(
        x = xplotval,
        y = results,
        xlab = xlab, 
        ylab = "Time elapsed (s)", 
        type = "b", 
        col = "blue", 
        lwd = 2,
        ylim = c(0, max(results))
    )
}

## below we check B, m, n and m + n, in order
## by varying that one (or both) parameters, and evaluating

## first we set some 
xd <- 5000 # default
xa <- seq(2, 10000, length.out = 10) # alternate
yd <- 5000 # default
ya <- seq(2, 10000, length.out = 10) # alternate
Bd <- 10
Ba <- seq(2, 100, length.out = 10)

## check B first
par(mfrow = c(2, 2))
check_complexity(xd, yd, Ba, Ba, xlab = "B") 
check_complexity(xa, yd, Bd, xa, xlab = "m") 
check_complexity(xd, ya, Bd, ya, xlab = "n") 
check_complexity(xa, ya, Bd, xa + ya, xlab = "m + n") 

## note it drops to 0 for B and m + n
## and is linear in B and m + n
## so it works out to O(B (m + n))
## (e.g. it can't be an m alone times everything,
## as m goes to 0, it goes to a constant)

