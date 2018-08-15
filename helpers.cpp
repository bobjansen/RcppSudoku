#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
IntegerMatrix subGrid(IntegerMatrix x, int i, int j) {
  i -= i % 3;
  j -= j % 3;
  return x(Range(i, i + 2), Range(j, j + 2));
}

// [[Rcpp::export]]
IntegerVector findChoicesCpp(const IntegerMatrix& x, int i, int j) {
  IntegerVector candidates(9);
  i--;
  j--;
  std::iota(candidates.begin(), candidates.end(), 1);
  return setdiff(setdiff(setdiff(candidates, IntegerVector(x(i, _))),
                         IntegerVector(x(_ , j))),
                 subGrid(x, i, j));
}

// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically 
// run after the compilation.
//

/*** R
findChoicesCpp(sudoku, 1, 1)
*/
