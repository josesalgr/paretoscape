#include "Package.h"
#include "OptimizationProblem.h"

// [[Rcpp::export]]
void rcpp_model_set_objective_vector(SEXP x,
                                     Rcpp::NumericVector obj,
                                     std::string model_sense = "min") {
  Rcpp::XPtr<OptimizationProblem> op(x);

  if ((int)obj.size() != (int)op->_obj.size()) {
    Rcpp::stop("Objective vector length does not match model dimension.");
  }

  op->_modelsense = model_sense;

  for (int j = 0; j < obj.size(); ++j) {
    op->_obj[j] = obj[j];
  }
}
