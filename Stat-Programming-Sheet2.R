##
## Q1
##

summarize <- function(x) {
  list(
    mean = mean(x), 
    median = median(x), 
    variance = var(x), 
    range = range(x)
  )
}
summarize(runif(100))

parsum <- function(x, n) {
  sq <- seq(from = 0, to = n)
  sum(exp( - x) * x^sq / factorial(sq))
}

ppois(15, lambda=10)
parsum(x=10, 15)
identical(ppois(15, lambda=10), parsum(x=10, 15))

printChar <- function(lst) {
  for (value in lst) {
    if (is.character(value)) {
      print(value)
    }
  }
}
printChar(list(c(1, 2, 3), c("this", "one"), c(4, 5, 6)))


random_walk <- function(k) {
  current_position <- 0
  out <- 0
  while (abs(current_position) < k) {
    current_position <- current_position + sample(c(1, -1), 1)
    out <- c(out, current_position)
  }
  out
}
set.seed(3)
random_walk(8) ## an example




##
## Q2
##

data <- read.table("hares.txt", header = TRUE)
cols <- c("#E69F00", "#56B4E9")
plot(
  x = data[, "year"],
  y = data[, "hare"], 
  type = "l",
  xlim = range(data[, "year"]),
  ylim = c(0, max(data[, c("lynx", "hare")])),
  xlab = "Year", 
  ylab = "Pelts (thousands)",
  main = "Annual pelts recovered by Hudson's Bay Company",
  axes = FALSE,
  col = cols[1],
  lwd = 2
)
points(
  data[, "year"],
  data[, "lynx"],
  type = "l", 
  col = cols[2],
  lwd = 2
)
axis(side = 1)
axis(side = 2)
legend(
  "topright", 
  legend = c("hares", "lynxes"), 
  col = cols,
  lwd = 2
)




##
## Q3
##

library(MASS)
data(beav1)

ma3 <- function(x) {
  n <- length(x)
  z <- vector("numeric", length = n - 2)
  
  for (i in 1:(n-2)) {
    z[i] <- (x[i] + x[i+1] + x[i+2]) / 3
  }
  
  return(z)
}

head(ma3(beav1[, "temp"]))

plot(beav1[, "temp"], type="l")
n <- length(beav1[, "temp"])
points(2:(n-1), ma3(beav1[, "temp"]), type = "l", col = 2)

ma <- function(x, k) {
  if (k == 1) {
    return(x)
  }
  n <- length(x)
  out <- x[-(1:(k - 1))] / k
  for (i in 2:k) {
    out <- out + 
      x[seq(from = k + 1 - i, to = n + 1 - i)] / k
  }
  out
}

plotMA <- function(x, k) {
  cbPalette <- c(
    "#999999", "#E69F00", "#56B4E9", "#009E73", 
    "#F0E442", "#0072B2", "#D55E00", "#CC79A7"
  )
  cols <- rep(cbPalette, length.out = length(k) + 1)
  plot(x, type = "l", col = cols[1], lwd = 2, ylab = "Temperature")
  n <- length(x)
  k <- k[k != 1]
  for (i in seq_along(k)) {
    tmp <- ma(x, k[i])
    points(
      seq(from = k[i] / 2, length = n - k[i] + 1, by = 1), 
      tmp, 
      type="l", 
      col = cols[i + 1], 
      lwd = 2
    )
  }
  legend("topleft", c("Original", paste0("MA-", k)), col = cols, lwd = 2)
}
plotMA(beav1[, "temp"], k = c(5, 10))





##
## Q4
##
phi <- function(x) {
  (3 <= x) & (x <= 5)
}
set.seed(7)
x <- rcauchy(10)
cbind(x, phi(x))

simulate_using_cauchy <- function(n) {
  x <- rcauchy(n = n)
  cumsum(phi(x)) / cumsum(rep(1, n))
}

simulate_using_uniform_is <- function(n) {
  y <- runif(n = n, min = 3, max = 5)
  w <- 2 * dcauchy(y)
  cumsum(phi(y) * w) / cumsum(rep(1, n))
}

set.seed(9)
plot(simulate_using_cauchy(100), col = "red", type = "l", ylim = c(0, 0.10))
points(simulate_using_uniform_is(100), col = "blue", type = "l")
abline(h = 1 / pi * (atan(5) - atan(3)), col = "black")
legend(
  "topright", 
  lwd = 1, 
  col = c("red", "blue", "black"), 
  legend = c("MC-Cauchy", "IS-Uniform", "Truth")
)
## the importance sampler looks better
## it converges to a value closer to the truth faster

plot_running_averages <- function(
    mc_estimate_using_cauchy,
    mc_estimate_using_uniform_is
) {
  cbPalette <- c(
    "#999999", "#E69F00", "#56B4E9", "#009E73", 
    "#F0E442", "#0072B2", "#D55E00", "#CC79A7"
  )
  plot(
    mc_estimate_using_cauchy,
    col = cbPalette[2],
    type = "l",
    ylim = c(0, 0.1),
    ylab = "Monte Carlo estimate",
    xlab = "n",
    lwd = 2
  )
  points(
    mc_estimate_using_uniform_is,
    col = cbPalette[3],
    type = "l", 
    lwd = 2
  )
  abline(h = 1 / pi * (atan(5) - atan(3)), col = cbPalette[4])
  legend(
    "topright",
    c("MC-Cauchy", "IS-Uniform", "Truth"),
    col = cbPalette[2:4],
    lwd = 2
  )
}

n <- 100
mc_estimate_using_cauchy <- simulate_using_cauchy(n)
mc_estimate_using_uniform_is <- simulate_using_uniform_is(n)
plot_running_averages(  
  mc_estimate_using_cauchy,
  mc_estimate_using_uniform_is
)

evaluate_is <- function(n) {
  mc_estimate_using_cauchy <- simulate_using_cauchy(n)
  mc_estimate_using_uniform_is <- simulate_using_uniform_is(n)
  plot_running_averages(    
    mc_estimate_using_cauchy,
    mc_estimate_using_uniform_is
  )
}
par(mfrow = c(2, 1))
evaluate_is(50)
evaluate_is(1000)
