#include <Rcpp.h>
#include <getMinIndex.h>

//' Get index of minimum value in integer vector
//'
//' @param v An integer vector.
//' @export
// [[Rcpp::export]]
int getMinIndex(Rcpp::IntegerVector v) {
  int min = v[0];
  int min_idx = 0;
  for (int i = 1; i < v.length(); ++i) {
    if (v[i] < min) { min_idx = i; }
  }
  return(min_idx);
}
