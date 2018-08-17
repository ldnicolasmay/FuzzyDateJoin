# right_fuzzy_date_join.R

#' A Function to Perform Right Outer Joins of Longitudinal Data
#'
#' This function allows you to join longitudinal data sets.
#' @param x X data frame.
#' @param y Y data frame.
#' @param x_id_col Name of column in X data frame with IDs.
#' @param y_id_col Name of column in Y data frame with IDs.
#' @param x_date_col Name of column in X data frame with dates.
#' @param y_date_col Name of column in Y data frame with dates.
#' @param y_intvl_less Number of days before Y date to fuzzy match to X date. Defaults to 0.
#' @param y_intvl_more Number of days after Y date to fuzzy match to X date. Defaults to 0.
#' @param keep_x_id Keep column in X data frame with IDs? Defaults to TRUE.
#' @keywords fuzzy, right, data, join, longitudinal
#' @export
#' @examples
#' # Define basic data frames X and Y
#' X <- data.frame(
#'   x_id = c(1L, 1L, 1L, 2L, 2L, 3L, 3L, 4L, 5L, 5L),
#'   x_date = as.Date(c("2015-06-01", "2016-06-28", "2017-05-25", "2014-02-23",
#'                      "2015-03-09", "2016-08-02", "2016-12-13", "2018-03-28",
#'                      "2017-05-17", "2018-04-03")),
#'   x_data = runif(10, min = 0, max = 10))
#'
#' Y <- data.frame(
#'   y_id = c(1L, 1L, 2L, 2L, 2L, 3L, 4L, 4L, 5L, 5L),
#'   y_date = as.Date(c("2015-06-01", "2017-05-20", "2014-02-23", "2015-03-10",
#'                      "2015-03-11", "2016-08-02", "2018-04-01", "2017-03-22",
#'                      "2017-05-16", "2017-05-17")),
#'   y_data = runif(10, min = -100, max = 0))
#'
#' # Use default values where possible
#' right_fuzzy_date_join(x = X, y = Y,
#'                       x_id = "x_id", y_id = "y_id",
#'                       x_date = "x_date", y_date = "y_date")
#'
#' # Define fuzzy date matching intervals and remove `y_id` column
#' intvl_less <- 5
#' intvl_more <- 3
#' right_fuzzy_date_join(x = X, y = Y,
#'                       x_id = "x_id", y_id = "y_id",
#'                       x_date = "x_date", y_date = "y_date",
#'                       y_intvl_less = intvl_less, y_intvl_more = intvl_more,
#'                       keep_x_id = FALSE)

# R FUNCTION - RIGHT FUZZY JOIN ----
#   *Note that this uses left_fuzzy_date_join_cpp, but swaps x and y
right_fuzzy_date_join <-

  function(x, y,
           x_id_col, y_id_col,
           x_date_col, y_date_col,
           y_intvl_less = 0, y_intvl_more = 0,
           keep_x_id = TRUE) {

    # Check inputs for correct classes
    if (!is.data.frame(x)) {
      stop("`x` must be of 'data.frame' class")
    }
    if (!is.data.frame(y)) {
      stop("`y` must be of 'data.frame' class")
    }
    if (class(x[[x_date_col]]) != "Date") {
      stop("`x_date_col` must be of 'Date' class")
    }
    if (class(y[[y_date_col]]) != "Date") {
      stop("`y_date_col` must be of 'Date' class")
    }

    # Create data frame that defines which rows from X and Y to keep
    Z_rows <-
      left_fuzzy_date_join_cpp(
        x = y, y = x,
        x_id_col = y_id_col, y_id_col = x_id_col,
        x_date_col = y_date_col, y_date_col = x_date_col,
        x_intvl_less = y_intvl_less,
        x_intvl_more = y_intvl_more)

    # Keep `y_id` column in return data frame?
    if (keep_x_id) {
      Z <- cbind(x[Z_rows$j_rows + 1, ],
                 y[Z_rows$i_rows + 1, ])
    } else {
      Z <- cbind(x[Z_rows$j_rows + 1, c(-1)],
                 y[Z_rows$i_rows + 1, ])
    }

    # Coerce `x_date` and `y_date` of return data frame back to Date class
    Z[[x_date_col]] <- as.Date(Z[[x_date_col]], origin = "1970-01-01")
    Z[[y_date_col]] <- as.Date(Z[[y_date_col]], origin = "1970-01-01")

    return(Z)
  }
