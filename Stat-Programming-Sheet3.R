##
## Q1
##
replicate(50, rt(10, df=5), simplify=FALSE)

mapply(rt, n = 10, df = 1:50)

lapply(1:20, seq)

set.seed(2022)
X <- matrix(rexp(200), 20, 10)

apply(X, 2, min)

sapply(CO2, is.numeric)

apply(CO2, 2, class)
apply(as.matrix(CO2), 2, class) ## same thing
head(as.matrix(CO2)) ## type-converted entire thing to character



##
## Q2
##

## we use skip = 2 to skip the first commented line 
## we specify sep = "\t" as the column names have spaces
system.time(baseR_gtex <- read.table("GTEx_analysis.txt.gz", skip = 2, sep = "\t", header = TRUE))

system.time(fread_gtex <- data.table::fread("GTEx_analysis.txt.gz"))

gtex_subset <- fread_gtex
# gtex_subset$Bladder <- NULL ## many other approaches work too
gtex_subset <- fread_gtex[, "Bladder" := NULL] ## many other approaches work too

system.time(save(gtex_subset, file = tempfile(), compress = FALSE))
system.time(save(gtex_subset, file = tempfile(), compress = TRUE))
system.time(data.table::fwrite(gtex_subset, file = tempfile()))


##
## Q3
##

spray_data <- read.table("sprays.txt", header = TRUE)
head(spray_data)
str(spray_data)

mean(spray_data[spray_data[, "spray"] == "A", "count"])

## or more generally
for(spray_type in c("A", "B", "C", "D", "E", "F")) {
    print(mean(spray_data[spray_data[, "spray"] == spray_type, "count"]))
}

## using tapply
tapply(spray_data[, "count"], spray_data[, "spray"], mean)

quantile(spray_data[, "count"]) ## how it works

tapply(spray_data[, "count"], spray_data[, "spray"], quantile)


##
## Q4
##

gtex <- data.table::fread("GTEx_analysis.txt.gz", data.table = FALSE)

set.seed(2022)
tissue_with_highest_expression <- apply(gtex[, -c(1:2)], 1, function(x) {
    sample(names(x)[x == max(x)], 1)
})

head(sort(table(tissue_with_highest_expression), decreasing = TRUE))
## testis wins by a mile!

results <- t(sapply(gtex[, -c(1:2)], function(x) {
    c(
        mean = mean(x),
        sd = sd(x),
        median = median(x),
        quantile(x, probs = c(0.05, 0.95))
    )
}))
head(results)


focal_entry <- as.numeric(gtex[gtex[, 2] == "APOB", -c(1:2)])

## normal way
system.time({
    results <- apply(gtex[, -c(1:2)], 1, function(x) {
        y <- x - focal_entry
        sum(y ** 2)
    })
})

## transposed way (ignore time for transposition)
## assume we do this once, then each lookup is faster
gtex_t <- data.frame(t(gtex[, -c(1:2)]))
system.time({
    results <- sapply(gtex_t, function(x) {
        y <- x - focal_entry
        sum(y ** 2)
    })
})

print(gtex[order(results)[1:11], 2])


heatmap(as.matrix(gtex[order(results)[1:10], -c(1:2)]))
heatmap(as.matrix(gtex[sample(1:nrow(gtex), 100), -c(1:2)]))

