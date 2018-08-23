# FuzzyDateJoin
Functions for joining longitudinal datasets by ID and fuzzy dates (+/- date ranges).

1. Install `FuzzyDateJoin` via the `devtools` package

```r
devtools::install_github('ldnicolasmay/FuzzyDateJoin')
```

2. Create tibbles X and Y with longitudinal data

```r
X <- data.frame(
  x_id = c(1L, 1L, 1L, 2L, 2L, 3L, 3L, 4L, 5L, 5L), 
  x_date = as.Date(c("2015-06-01", "2016-06-28", "2017-05-25", "2014-02-23",
                     "2015-03-09", "2016-08-02", "2016-12-13", "2018-03-28",
                     "2017-05-17", "2018-04-03")), 
  x_data = runif(10, min = 0, max = 10))
  
Y <- data.frame(
  y_id = c(1L, 1L, 2L, 2L, 2L, 3L, 4L, 4L, 5L, 5L), 
  y_date = as.Date(c("2015-06-01", "2017-05-20", "2014-02-23", "2015-03-10", 
                     "2015-03-11", "2016-08-02", "2018-04-01", "2017-03-22", 
                     "2017-05-16", "2017-05-17")), 
  y_data = runif(10, min = -100, max = 0))
```

3. Use the included functions to do data joins based on IDs and day ranges

```r
intvl_less <- 5
intvl_more <- 3
inner_fuzzy_date_join(x = X, y = Y,
                      x_id_col = "x_id", y_id_col = "y_id",
                      x_date_col = "x_date", y_date_col = "y_date",
                      x_intvl_less = intvl_less, x_intvl_more = intvl_more,
                      keep_y_id = FALSE
                      )

left_fuzzy_date_join(x = X, y = Y,
                     x_id_col = "x_id", y_id_col = "y_id",
                     x_date_col = "x_date", y_date_col = "y_date" #,
                     # x_intvl_less = 0, x_intvl_more = 0,
                     # keep_y_id = TRUE
                     )

intvl_less <- 1
intvl_more <- 10
right_fuzzy_date_join(x = X, y = Y,
                      x_id_col = "x_id", y_id_col = "y_id",
                      x_date_col = "x_date", y_date_col = "y_date",
                      y_intvl_less = intvl_less, y_intvl_more = intvl_more,
                      keep_x_id = FALSE
                      )
```
