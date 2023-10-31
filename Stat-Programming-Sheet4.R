##
## Q1
##
library("tidyverse")
url <- paste0( 
    "https://raw.githubusercontent.com/hadley/", 
    "ggplot2movies/master/data-raw/movies.csv"
)
system.time(movies <- read_csv(url, progress = FALSE))
system.time(temp <- read.csv(url))

movies_description <- read_csv("movies_description.csv", progress = FALSE)
as.data.frame(movies_description)



##
## Q2
##
str(movies)
nrow(movies)
ncol(movies)

movies %>% group_by(mpaa) %>% summarize(count = length(mpaa))

base_mpaa <- movies %>% pull(mpaa)
table(base_mpaa, useNA = "always")



##
## Q3
##


movies %>% 
    ggplot(aes(x = rating)) + 
    geom_histogram()

movies %>% 
    ggplot(aes(x = budget)) + 
    geom_histogram()

movies %>% 
    ggplot(aes(x = budget, y = rating)) +
    geom_point() + 
    scale_x_continuous(trans='log10')

  
summ <- lm(rating ~ budget, data = movies)
coefficients(summary(summ))
## p > 0.05, we cannot reject the null hypothesis of no association



##
## Q4
##

movies %>% 
    arrange(desc(budget)) %>% 
    head(n = 2) %>% 
    select(title, budget) 

movies %>% 
    filter(year >= 1990 & year <= 1999) %>% 
    arrange(desc(budget)) %>% 
    head(n = 3) %>% 
    select(title, budget, year)

movies %>% 
    group_by(title) %>% 
    summarize(n = length(title)) %>% 
    arrange(desc(n)) %>% 
    head

title <- movies %>% pull(title) ## to get a base R character vector
head(sort(table(title), decreasing = TRUE))

## as to why, the most common movie is Alice in Wonderland. 
## querying this, we see different years, so multiple entries suggests remakes
movies %>% filter(title == "Alice in Wonderland")



##
## Q5
##

movies %>% 
    mutate(
        category_count = 
            Action + Animation + Comedy + Drama + 
            Documentary + Romance + Short
    ) %>% 
    filter(category_count == 1) %>% 
    nrow

budget_by_category <- movies %>% 
    mutate(
        category_count = 
            Action + Animation + Comedy + Drama + 
            Documentary + Romance + Short
    ) %>% 
    filter(category_count == 1) %>% 
    gather(
        key = category, value = category_index, 
        Action, Animation, Comedy, Drama, Documentary, Romance, Short
    ) %>% 
    filter(category_index == 1) %>% 
    group_by(category) %>% 
    summarise(mean = mean(budget, na.rm = TRUE), n = length(budget))
budget_by_category


budget_by_category %>% 
    ggplot(aes(x=category, y=mean)) +
  geom_bar(stat="identity")



##
## Q6
##

presidents <- read_csv("presidents.csv")

movies_plus <- left_join(movies, presidents, by = "year")

movies_plus %>% 
    group_by(party) %>% 
    summarise(mean = mean(rating, na.rm = TRUE), n = length(rating))    

movies_plus %>% 
    group_by(president) %>% 
    summarise(mean = mean(length, na.rm = TRUE)) %>%
    arrange(desc(mean)) %>% 
    head(n = 3)


presidents_raw <- read_csv("presidents_raw.csv")
new_presidents <- presidents_raw %>% 
    separate(
        col = "took_office", 
        into = c("day", "month", "from_year"), 
        sep = "/"
    ) %>% 
    separate(
        col = "left_office", 
        into = c("day", "month", "to_year"), 
        sep = "/"
    ) %>% 
    mutate_at("from_year", as.numeric) %>% 
    mutate_at("to_year", as.numeric) %>% 
    mutate(years_in_office = to_year - from_year) %>% 
    select(president, party, from_year, to_year, years_in_office) %>% 
    uncount(years_in_office, .remove = FALSE, .id = "years_elapsed") %>% 
    mutate(year = from_year + years_elapsed - 1)

testthat::expect_equivalent(presidents, new_presidents)
