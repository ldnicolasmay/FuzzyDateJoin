# test_duplicate_col_names.R

context('Data frame column names')
library(FuzzyDateJoin)

test_that('Column names duplicated between X and Y are changed appropriately', {

  # Define input data frames X and Y ----
  X <- data.frame(
    id = c(1L),
    date = as.Date(c('2015-06-01')),
    data = runif(1, min = 0, max = 10))
  Y <- data.frame(
    id = c(1L),
    date = as.Date(c('2015-06-01')),
    data = runif(1, min = -100, max = 0))

  # Derive joined data frames Z_* using inner, left, and right fuzzy joins ----
  Z_inner <-
    inner_fuzzy_date_join(x = X, y = Y,
                          x_id_col = 'id', y_id_col = 'id',
                          x_date_col = 'date', y_date_col = 'date',
                          x_intvl_less = 0, x_intvl_more = 0,
                          keep_y_id = TRUE)
  Z_left <-
    left_fuzzy_date_join(x = X, y = Y,
                         x_id_col = 'id', y_id_col = 'id',
                         x_date_col = 'date', y_date_col = 'date',
                         x_intvl_less = 0, x_intvl_more = 0,
                         keep_y_id = TRUE)
  Z_right <-
    right_fuzzy_date_join(x = X, y = Y,
                          x_id_col = 'id', y_id_col = 'id',
                          x_date_col = 'date', y_date_col = 'date',
                          y_intvl_less = 0, y_intvl_more = 0,
                          keep_x_id = TRUE)

  # Define expectations ----
  expect_equal(names(Z_inner),
               c('id_x', 'date_x', 'data_x', 'id_y', 'date_y', 'data_y'))
  expect_equal(names(Z_left),
               c('id_x', 'date_x', 'data_x', 'id_y', 'date_y', 'data_y'))
  expect_equal(names(Z_right),
               c('id_x', 'date_x', 'data_x', 'id_y', 'date_y', 'data_y'))
})

