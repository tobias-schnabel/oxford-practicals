library("knitr")
knit("robbie_mwe.rnw")
system("pdflatex robbie_mwe.tex")


library("knitr")
## system(paste0("R -e 'knitr::knit(", shQuote("'movies_report.rnw'"), ")'"))
knit("movies_report.rnw")
system("pdflatex movies_report.tex")
