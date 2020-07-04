ST558 Project 2
================
Jessica Speer
July 3, 2020

### The code used to automate rmarkdown files:

``` r
dayvars <- c("monday", "tuesday")
output_file <- paste0(dayvars, ".md")
params = lapply(dayvars, FUN = function(x){list(day = x)})
reports <- tibble(output_file, params)
pwalk(reports, render, input = "C:\\Users\\jessi\\Documents\\ST 558\\project2\\project2\\st558proj2daily.Rmd")
```

### Links to reports

[Monday is available here](monday.md)
