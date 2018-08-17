// fuzzy_date_join_rcpp.cpp

#include <Rcpp.h>
using namespace Rcpp;

// getMinIndex
int getMinIndex(IntegerVector v) {
  int min = v[0];
  int min_idx = 0;
  for (int i = 1; i < v.length(); ++i) {
    if (v[i] < min) { min_idx = i; }
  }
  return(min_idx);
}

//' Inner fuzzy date join
//'
//' @param x X data frame.
//' @param y Y data frame.
//' @param x_id_col Name of column in X data frame with IDs.
//' @param y_id_col Name of column in Y data frame with IDs.
//' @param x_date_col Name of column in X data frame with dates.
//' @param y_date_col Name of column in Y data frame with dates.
//' @param x_intvl_less Number of days before X date to match to Y date. Defaults to 0.
//' @param x_intvl_more Number of days after X date to match to Y date. Defaults to 0.
//' @export
// [[Rcpp::export]]
DataFrame inner_fuzzy_date_join_cpp(DataFrame x,
                                    DataFrame y,
                                    String x_id_col,
                                    String y_id_col,
                                    String x_date_col,
                                    String y_date_col,
                                    int x_intvl_less = 0,
                                    int x_intvl_more = 0) {
  int x_n_row = x.nrows(); // int x_n_col = x.size();
  int y_n_row = y.nrows(); // int y_n_col = y.size();

  // Get x ids / dates
  StringVector x_id_orig = x[x_id_col];
  IntegerVector x_date_orig = x[x_date_col];
  // Get y ids / dates
  StringVector y_id_orig = y[y_id_col];
  IntegerVector y_date_orig = y[y_date_col];
  // Create x intervals
  IntegerVector x_intvl_beg(x_n_row);
  IntegerVector x_intvl_end(x_n_row);
  for (int i = 0; i < x_n_row; ++i) {
    x_intvl_beg[i] = x_date_orig[i] - x_intvl_less;
    x_intvl_end[i] = x_date_orig[i] + x_intvl_more;
  }

  // Create vectors for storing id-matching data temporarily: x y temp => xyt
  StringVector xyt_id = StringVector::create();
  IntegerVector xyt_x_date = IntegerVector::create();
  IntegerVector xyt_y_date = IntegerVector::create();
  IntegerVector xyt_x_y_date_diff = IntegerVector::create();
  // Create `xyt` vectors for which x rows (i) to match to y rows (j)
  IntegerVector xyt_i_row = IntegerVector::create();
  IntegerVector xyt_j_row = IntegerVector::create();

  // Create vectors for storing Z data
  StringVector id = StringVector::create();
  IntegerVector x_date = IntegerVector::create();
  IntegerVector y_date = IntegerVector::create();
  IntegerVector x_y_date_diff = IntegerVector::create();
  // Create Z vectors for which x rows (i) to match to y rows (j)
  IntegerVector i_row = IntegerVector::create();
  IntegerVector j_row = IntegerVector::create();

  // Loop over x
  for (int i = 0; i < x_n_row; ++i) {
    // Reset temporary id-matching data vectors
    StringVector xyt_id = StringVector::create();
    IntegerVector xyt_x_date = IntegerVector::create();
    IntegerVector xyt_y_date = IntegerVector::create();
    IntegerVector xyt_x_y_date_diff = IntegerVector::create();
    IntegerVector xyt_i_row = IntegerVector::create();
    IntegerVector xyt_j_row = IntegerVector::create();
    // Loop over y
    for (int j = 0; j < y_n_row; ++j) {

      if (x_id_orig[i] == y_id_orig[j] &&
          (y_date_orig[j] >= x_intvl_beg[i] && y_date_orig[j] <= x_intvl_end[i]) ) {
        // Add matches to `xyt` vectors
        xyt_id.push_back(x_id_orig[i]);
        xyt_x_date.push_back(x_date_orig[i]);
        xyt_y_date.push_back(y_date_orig[j]);
        xyt_x_y_date_diff.push_back(abs(x_date_orig[i] - y_date_orig[j]));
        xyt_i_row.push_back(i);
        xyt_j_row.push_back(j);
      }
    }
    if (xyt_id.length() > 0) {
      // Get index of minimum x<=>y date difference
      int x_y_date_diff_min_idx = getMinIndex(xyt_x_y_date_diff);
      // Add minimum `xyt` values to Z
      id.push_back(xyt_id[x_y_date_diff_min_idx]);
      x_date.push_back(xyt_x_date[x_y_date_diff_min_idx]);
      y_date.push_back(xyt_y_date[x_y_date_diff_min_idx]);
      x_y_date_diff.push_back(xyt_x_y_date_diff[x_y_date_diff_min_idx]);
      i_row.push_back(xyt_i_row[x_y_date_diff_min_idx]);
      j_row.push_back(xyt_j_row[x_y_date_diff_min_idx]);
    }
  }

  return(
    DataFrame::create(
      _["id"] = id,
      _["x_date"] = x_date,
      _["y_date"] = y_date,
      _["x_y_date_diff"] = x_y_date_diff,
      _["i_rows"] = i_row,
      _["j_rows"] = j_row
    )
  );
}

//' Left fuzzy date join
//'
//' @param x X data frame.
//' @param y Y data frame.
//' @param x_id_col Name of column in X data frame with IDs.
//' @param y_id_col Name of column in Y data frame with IDs.
//' @param x_date_col Name of column in X data frame with dates.
//' @param y_date_col Name of column in Y data frame with dates.
//' @param x_intvl_less Number of days before X date to match to Y date. Defaults to 0.
//' @param x_intvl_more Number of days after X date to match to Y date. Defaults to 0.
//' @export
// [[Rcpp::export]]
DataFrame left_fuzzy_date_join_cpp(DataFrame x,
                                   DataFrame y,
                                   String x_id_col,
                                   String y_id_col,
                                   String x_date_col,
                                   String y_date_col,
                                   int x_intvl_less = 0,
                                   int x_intvl_more = 0) {
  int x_n_row = x.nrows(); // int x_n_col = x.size();
  int y_n_row = y.nrows(); // int y_n_col = y.size();

  // Get x ids / dates
  StringVector x_id_orig = x[x_id_col];
  IntegerVector x_date_orig = x[x_date_col];
  // Get y ids / dates
  StringVector y_id_orig = y[y_id_col];
  IntegerVector y_date_orig = y[y_date_col];
  // Create x intervals
  IntegerVector x_intvl_beg(x_n_row);
  IntegerVector x_intvl_end(x_n_row);
  for (int i = 0; i < x_n_row; ++i) {
    x_intvl_beg[i] = x_date_orig[i] - x_intvl_less;
    x_intvl_end[i] = x_date_orig[i] + x_intvl_more;
  }

  // Create vectors for storing id-matching data temporarily: x y temp => xyt
  StringVector xyt_id = StringVector::create();
  IntegerVector xyt_x_date = IntegerVector::create();
  IntegerVector xyt_y_date = IntegerVector::create();
  IntegerVector xyt_x_y_date_diff = IntegerVector::create();
  // Create `xyt` vectors for which x rows (i) to match to y rows (j)
  IntegerVector xyt_i_row = IntegerVector::create();
  IntegerVector xyt_j_row = IntegerVector::create();

  // Create vectors for storing Z data
  StringVector id = StringVector::create();
  IntegerVector x_date = IntegerVector::create();
  IntegerVector y_date = IntegerVector::create();
  IntegerVector x_y_date_diff = IntegerVector::create();
  // Create Z vectors for which x rows (i) to match to y rows (j)
  IntegerVector i_row = IntegerVector::create();
  IntegerVector j_row = IntegerVector::create();

  // Loop over x
  for (int i = 0; i < x_n_row; ++i) {
    // Reset temporary id-matching data vectors
    StringVector xyt_id = StringVector::create();
    IntegerVector xyt_x_date = IntegerVector::create();
    IntegerVector xyt_y_date = IntegerVector::create();
    IntegerVector xyt_x_y_date_diff = IntegerVector::create();
    IntegerVector xyt_i_row = IntegerVector::create();
    IntegerVector xyt_j_row = IntegerVector::create();
    // Loop over y
    for (int j = 0; j < y_n_row; ++j) {

      if (x_id_orig[i] == y_id_orig[j] &&
          (y_date_orig[j] >= x_intvl_beg[i] && y_date_orig[j] <= x_intvl_end[i]) ) {
        // Add matches to `xyt` vectors
        xyt_id.push_back(x_id_orig[i]);
        xyt_x_date.push_back(x_date_orig[i]);
        xyt_y_date.push_back(y_date_orig[j]);
        xyt_x_y_date_diff.push_back(abs(x_date_orig[i] - y_date_orig[j]));
        xyt_i_row.push_back(i);
        xyt_j_row.push_back(j);
      }
    }
    if (xyt_id.length() > 0) {
      // Get index of minimum x<=>y date difference
      int x_y_date_diff_min_idx = getMinIndex(xyt_x_y_date_diff);
      // Add minimum `xyt` values to Z
      id.push_back(xyt_id[x_y_date_diff_min_idx]);
      x_date.push_back(xyt_x_date[x_y_date_diff_min_idx]);
      y_date.push_back(xyt_y_date[x_y_date_diff_min_idx]);
      x_y_date_diff.push_back(xyt_x_y_date_diff[x_y_date_diff_min_idx]);
      i_row.push_back(xyt_i_row[x_y_date_diff_min_idx]);
      j_row.push_back(xyt_j_row[x_y_date_diff_min_idx]);
    } else {
      id.push_back(x_id_orig[i]);
      x_date.push_back(x_date_orig[i]);
      y_date.push_back(NA_INTEGER);
      x_y_date_diff.push_back(NA_INTEGER);
      i_row.push_back(i);
      j_row.push_back(NA_INTEGER);
    }
  }

  return(
    DataFrame::create(
      _["id"] = id,
      _["x_date"] = x_date,
      _["y_date"] = y_date,
      _["x_y_date_diff"] = x_y_date_diff,
      _["i_rows"] = i_row,
      _["j_rows"] = j_row
    )
  );
}


/* * * * * * * *
 * * * * * * * *
 * EXTRA SPACE *
 * * * * * * * *
 * * * * * * * */

