# test_date_intervals.R

context('Date intervals')
library(FuzzyDateJoin)

test_that('Date intervals `*_intvl_less` and `*_intvl_more` work appropriately', {

  # Define input data frames X and Y ----
  X <- data.frame(
    id   = c(1L),
    date = as.Date(c('2015-06-01')),
    data = c(10.0))
  Y <- data.frame(
    id   = c(1L),
    date = as.Date(c('2015-06-02')),
    data = c(-10.0))


  # Derive joined data frames Z_* using inner, left, and right fuzzy joins ----

  # _ Inner ----
  Z_inner_fuzzy_match <-
    inner(x = X, y = Y,
          x_id_col = 'id', y_id_col = 'id',
          x_date_col = 'date', y_date_col = 'date',
          x_intvl_less = 1, x_intvl_more = 1,
          keep_y_id = TRUE)
  Z_inner_exact_match <-
    inner(x = X, y = X,
          x_id_col = 'id', y_id_col = 'id',
          x_date_col = 'date', y_date_col = 'date',
          x_intvl_less = 0, x_intvl_more = 0,
          keep_y_id = TRUE)
  Z_inner_mis_match <-
    inner(x = X, y = Y,
          x_id_col = 'id', y_id_col = 'id',
          x_date_col = 'date', y_date_col = 'date',
          x_intvl_less = 0, x_intvl_more = 0,
          keep_y_id = TRUE)
  # _ Outer Left ----
  Z_outer_left_fuzzy_match <-
    outer_left(x = X, y = Y,
               x_id_col = 'id', y_id_col = 'id',
               x_date_col = 'date', y_date_col = 'date',
               x_intvl_less = 1, x_intvl_more = 1,
               keep_y_id = TRUE)
  Z_outer_left_exact_match <-
    outer_left(x = X, y = X,
               x_id_col = 'id', y_id_col = 'id',
               x_date_col = 'date', y_date_col = 'date',
               x_intvl_less = 0, x_intvl_more = 0,
               keep_y_id = TRUE)
  Z_outer_left_mis_match <-
    outer_left(x = X, y = Y,
               x_id_col = 'id', y_id_col = 'id',
               x_date_col = 'date', y_date_col = 'date',
               x_intvl_less = 0, x_intvl_more = 0,
               keep_y_id = TRUE)

  # _ Outer Right ----
  Z_outer_right_fuzzy_match <-
    outer_right(x = X, y = Y,
                x_id_col = 'id', y_id_col = 'id',
                x_date_col = 'date', y_date_col = 'date',
                y_intvl_less = 1, y_intvl_more = 1,
                keep_x_id = TRUE)
  Z_outer_right_exact_match <-
    outer_right(x = X, y = X,
                x_id_col = 'id', y_id_col = 'id',
                x_date_col = 'date', y_date_col = 'date',
                y_intvl_less = 0, y_intvl_more = 0,
                keep_x_id = TRUE)
  Z_outer_right_mis_match <-
    outer_right(x = X, y = Y,
                x_id_col = 'id', y_id_col = 'id',
                x_date_col = 'date', y_date_col = 'date',
                y_intvl_less = 0, y_intvl_more = 0,
                keep_x_id = TRUE)
  # Empty data frame with appropriate column names
  empty_df <-
    setNames(
      object = data.frame(matrix(ncol = 6, nrow = 0)),
      nm = c('id_x', 'date_x', 'data_x',
             'id_y', 'date_y', 'data_y')
    )
  # Reclass empty_df columns to match output Z_inner_mis_match columns
  empty_df[, 'id_x']   <- as.integer(empty_df[, 'id_x'])
  empty_df[, 'date_x'] <- as.Date(empty_df[, 'date_x'])
  empty_df[, 'data_x'] <- as.numeric(empty_df[, 'id_x'])
  empty_df[, 'id_y']   <- as.integer(empty_df[, 'id_y'])
  empty_df[, 'date_y'] <- as.Date(empty_df[, 'date_y'])
  empty_df[, 'data_y'] <- as.numeric(empty_df[, 'data_y'])


  # Define expectations ----

  # _ Inner ----
  expect_equal(
    Z_inner_fuzzy_match,
    data.frame(
      id_x   = c(1L),
      date_x = as.Date(c('2015-06-01')),
      data_x = c(10.0),
      id_y   = c(1L),
      date_y = as.Date(c('2015-06-02')),
      data_y = c(-10.0)
    ))
  expect_equal(
    Z_inner_exact_match,
    data.frame(
      id_x   = c(1L),
      date_x = as.Date(c('2015-06-01')),
      data_x = c(10.0),
      id_y   = c(1L),
      date_y = as.Date(c('2015-06-01')),
      data_y = c(10.0)
    ))
  expect_equal(
    Z_inner_mis_match,
    empty_df)

  # _ Outer Left ----
  expect_equal(
    Z_outer_left_fuzzy_match,
    data.frame(
      id_x   = c(1L),
      date_x = as.Date(c('2015-06-01')),
      data_x = c(10.0),
      id_y   = c(1L),
      date_y = as.Date(c('2015-06-02')),
      data_y = c(-10.0)
    ))
  expect_equal(
    Z_outer_left_exact_match,
    data.frame(
      id_x   = c(1L),
      date_x = as.Date(c('2015-06-01')),
      data_x = c(10.0),
      id_y   = c(1L),
      date_y = as.Date(c('2015-06-01')),
      data_y = c(10.0)
    ))
  expect_equal(
    Z_outer_left_mis_match,
    data.frame(
      id_x   = c(1L),
      date_x = as.Date(c('2015-06-01')),
      data_x = c(10.0),
      id_y   = c(NA_integer_),
      date_y = as.Date(c(NA_integer_), origin = '1970-01-01'),
      data_y = c(NA_real_)
    ))

  # _ Outer Right ----
  expect_equal(
    Z_outer_right_fuzzy_match,
    data.frame(
      id_x   = c(1L),
      date_x = as.Date(c('2015-06-01')),
      data_x = c(10.0),
      id_y   = c(1L),
      date_y = as.Date(c('2015-06-02')),
      data_y = c(-10.0)
    ))
  expect_equal(
    Z_outer_right_exact_match,
    data.frame(
      id_x   = c(1L),
      date_x = as.Date(c('2015-06-01')),
      data_x = c(10.0),
      id_y   = c(1L),
      date_y = as.Date(c('2015-06-01')),
      data_y = c(10.0)
    ))
  expect_equal(
    Z_outer_right_mis_match,
    data.frame(
      id_x   = c(NA_integer_),
      date_x = as.Date(c(NA_integer_), origin = '1970-01-01'),
      data_x = c(NA_real_),
      id_y   = c(1L),
      date_y = as.Date(c('2015-06-02')),
      data_y = c(-10.0)
    ))

})

