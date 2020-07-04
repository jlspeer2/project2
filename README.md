ST558 Project 2
================
Jessica Speer
July 3, 2020

### Links to reports

[General (data exploration) is available here](st558proj2.md)

[Monday is available here](monday.md)
[Tuesday is available here](tuesday.md)
[Wednesday is available here](wednesday.md)
[Thursday is available here](thursday.md)
[Friday is available here](friday.md)
[Saturday is available here](saturday.md)
[Sunday is available here](sunday.md)

### The code used to automate rmarkdown files:

``` r
dayvars <- c("monday", "tuesday", "wednesday", "thursday", "friday", "saturday", "sunday")
output_file <- paste0(dayvars, ".md")
params = lapply(dayvars, FUN = function(x){list(day = x)})
reports <- tibble(output_file, params)
pwalk(reports, render, input = "C:\\Users\\jessi\\Documents\\ST 558\\project2\\project2\\st558proj2daily.Rmd")
```
