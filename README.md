# FuzzyDateJoin
R package containing functions for joining longitudinal datasets by ID and fuzzy dates (+/- date ranges).

## Installing `FuzzyDateJoin`

Install `FuzzyDateJoin` via the `devtools` package.

```r
devtools::install_github('ldnicolasmay/FuzzyDateJoin')
```

## Using `FuzzyDateJoin`

1. Create data frames X and Y with longitudinal data.

```r
library(FuzzyDateJoin)

set.seed(1)
X <- data.frame(
  x_id   = c(1L, 1L, 1L, 2L, 2L, 3L, 3L, 4L, 5L, 5L), 
  x_date = as.Date(c("2015-06-01", "2016-06-28", "2017-05-25", "2014-02-23",
                     "2015-03-09", "2016-08-02", "2016-12-13", "2018-03-28",
                     "2017-05-17", "2018-04-03")), 
  x_data = runif(10, min = 0, max = 10))
  
Y <- data.frame(
  y_id   = c(1L, 1L, 2L, 2L, 2L, 3L, 4L, 4L, 5L, 5L), 
  y_date = as.Date(c("2015-06-01", "2017-05-20", "2014-02-23", "2015-03-10", 
                     "2015-03-11", "2016-08-02", "2018-04-01", "2017-03-22", 
                     "2017-05-16", "2017-05-17")), 
  y_data = runif(10, min = -100, max = 0))
```

2. Use the included functions to do data joins based on IDs and day ranges.

```r
intvl_less <- 5
intvl_more <- 3
inner(x = X, y = Y,
      x_id_col = "x_id", y_id_col = "y_id",
      x_date_col = "x_date", y_date_col = "y_date",
      x_intvl_less = intvl_less, x_intvl_more = intvl_more,
      keep_y_id = FALSE
      )

##   x_id     x_date   x_data     y_date     y_data
## 1    1 2015-06-01 2.655087 2015-06-01  -6.529477
## 2    1 2017-05-25 5.728534 2017-05-20 -78.785748
## 3    2 2014-02-23 9.082078 2014-02-23 -34.832623
## 4    2 2015-03-09 2.016819 2015-03-10 -87.444490
## 5    3 2016-08-02 8.983897 2016-08-02 -61.388591
## 6    5 2017-05-17 6.291140 2017-05-17 -65.965100

outer_left(x = X, y = Y,
           x_id_col = "x_id", y_id_col = "y_id",
           x_date_col = "x_date", y_date_col = "y_date" #,
           # x_intvl_less = 0, x_intvl_more = 0,
           # keep_y_id = TRUE
           )

##    x_id     x_date    x_data y_id     y_date     y_data
## 1     1 2015-06-01 2.6550866    1 2015-06-01  -6.529477
## 2     1 2016-06-28 3.7212390   NA       <NA>         NA
## 3     1 2017-05-25 5.7285336   NA       <NA>         NA
## 4     2 2014-02-23 9.0820779    2 2014-02-23 -34.832623
## 5     2 2015-03-09 2.0168193   NA       <NA>         NA
## 6     3 2016-08-02 8.9838968    3 2016-08-02 -61.388591
## 7     3 2016-12-13 9.4467527   NA       <NA>         NA
## 8     4 2018-03-28 6.6079779   NA       <NA>         NA
## 9     5 2017-05-17 6.2911404    5 2017-05-17 -65.965100
## 10    5 2018-04-03 0.6178627   NA       <NA>         NA

intvl_less <- 1
intvl_more <- 10
outer_right(x = X, y = Y,
            x_id_col = "x_id", y_id_col = "y_id",
            x_date_col = "x_date", y_date_col = "y_date",
            y_intvl_less = intvl_less, y_intvl_more = intvl_more,
            keep_x_id = FALSE
            )

##        x_date   x_data y_id     y_date     y_data
## 1  2015-06-01 2.655087    1 2015-06-01  -6.529477
## 2  2017-05-25 5.728534    1 2017-05-20 -78.785748
## 3  2014-02-23 9.082078    2 2014-02-23 -34.832623
## 4  2015-03-09 2.016819    2 2015-03-10 -87.444490
## 5        <NA>       NA    2 2015-03-11 -73.277933
## 6  2016-08-02 8.983897    3 2016-08-02 -61.388591
## 7        <NA>       NA    4 2018-04-01 -98.660967
## 8        <NA>       NA    4 2017-03-22 -61.761204
## 9  2017-05-17 6.291140    5 2017-05-16 -13.030915
## 10 2017-05-17 6.291140    5 2017-05-17 -65.965100

```
