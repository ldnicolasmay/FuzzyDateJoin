# test_keep_id_col.R

context('Keep secondary ID column')
library(FuzzyDateJoin)

test_that('Secondary ID columns are retained or removed appropriately', {

  # Define input data frames X and Y ----
  X <- data.frame(
    id   = c(1L),
    date = as.Date(c('2015-06-01')),
    data = runif(1, min = 0, max = 10))
  Y <- data.frame(
    id   = c(1L),
    date = as.Date(c('2015-06-01')),
    data = runif(1, min = -100, max = 0))

  # Derive joined data frames Z_* using inner, left, and right fuzzy joins ----
  Z_inner_keep_y <-
    inner(
      x = X, y = Y,
      x_id_col = 'id', y_id_col = 'id',
      x_date_col = 'date', y_date_col = 'date',
      x_intvl_less = 0, x_intvl_more = 0,
      keep_y_id = TRUE)
  Z_inner_remove_y <-
    inner(
      x = X, y = Y,
      x_id_col = 'id', y_id_col = 'id',
      x_date_col = 'date', y_date_col = 'date',
      x_intvl_less = 0, x_intvl_more = 0,
      keep_y_id = FALSE)
  Z_outer_left_keep_y <-
    outer_left(
      x = X, y = Y,
      x_id_col = 'id', y_id_col = 'id',
      x_date_col = 'date', y_date_col = 'date',
      x_intvl_less = 0, x_intvl_more = 0,
      keep_y_id = TRUE)
  Z_outer_left_remove_y <-
    outer_left(
      x = X, y = Y,
      x_id_col = 'id', y_id_col = 'id',
      x_date_col = 'date', y_date_col = 'date',
      x_intvl_less = 0, x_intvl_more = 0,
      keep_y_id = FALSE)
  Z_outer_right_keep_x <-
    outer_right(
      x = X, y = Y,
      x_id_col = 'id', y_id_col = 'id',
      x_date_col = 'date', y_date_col = 'date',
      y_intvl_less = 0, y_intvl_more = 0,
      keep_x_id = TRUE)
  Z_outer_right_remove_x <-
    outer_right(
      x = X, y = Y,
      x_id_col = 'id', y_id_col = 'id',
      x_date_col = 'date', y_date_col = 'date',
      y_intvl_less = 0, y_intvl_more = 0,
      keep_x_id = FALSE)

  # Define expectations ----
  expect_equal(names(Z_inner_keep_y),
               c('id_x', 'date_x', 'data_x', 'id_y', 'date_y', 'data_y'))
  expect_equal(names(Z_inner_remove_y),
               c('id_x', 'date_x', 'data_x', 'date_y', 'data_y'))
  expect_equal(names(Z_outer_left_keep_y),
               c('id_x', 'date_x', 'data_x', 'id_y', 'date_y', 'data_y'))
  expect_equal(names(Z_outer_left_remove_y),
               c('id_x', 'date_x', 'data_x', 'date_y', 'data_y'))
  expect_equal(names(Z_outer_right_keep_x),
               c('id_x', 'date_x', 'data_x', 'id_y', 'date_y', 'data_y'))
  expect_equal(names(Z_outer_right_remove_x),
               c('date_x', 'data_x', 'id_y', 'date_y', 'data_y'))
})







