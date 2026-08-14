#include "Package.h"
#include "OptimizationProblem.h"

#include <string>
#include <cmath>
#include <unordered_map>
#include <algorithm>
#include <cstdint>
#include <vector>

// helper: undirected key (i<j), i/j 1-based
static inline std::uint64_t edge_key_undirected(int i, int j) {
  return (static_cast<std::uint64_t>(static_cast<std::uint32_t>(i)) << 32) |
    static_cast<std::uint32_t>(j);
}

// [[Rcpp::export]]
Rcpp::List rcpp_add_objective_min_fragmentation(
    SEXP x,
    Rcpp::DataFrame relation_data,
    double weight = 1.0,
    double weight_multiplier = 1.0,
    std::string block_name = "objective_add_min_fragmentation",
    std::string tag = ""
) {
  Rcpp::XPtr<OptimizationProblem> op = Rcpp::as<Rcpp::XPtr<OptimizationProblem>>(x);

  if (op->ncol_used() == 0) {
    Rcpp::stop("Model has zero variables. Build base variables first.");
  }

  if (!std::isfinite(weight)) {
    Rcpp::stop("weight must be finite.");
  }
  if (!std::isfinite(weight_multiplier) || weight_multiplier < 0.0) {
    Rcpp::stop("weight_multiplier must be finite and >= 0.");
  }

  if (weight == 0.0) {
    return Rcpp::List::create(
      Rcpp::Named("ok") = true,
      Rcpp::Named("skipped") = true,
      Rcpp::Named("reason") = "weight==0"
    );
  }

  // must have been prepared (y variables exist)
  if (op->_n_y_pu <= 0 || op->_y_pu_offset < 0) {
    Rcpp::stop(
      "PU-fragmentation auxiliaries do not exist (missing y_pu block). "
      "Call the corresponding prepare() first."
    );
  }

  for (auto nm : {"internal_pu1", "internal_pu2", "weight"}) {
    if (!relation_data.containsElementNamed(nm)) {
      Rcpp::stop(std::string("relation_data must contain column '") + nm + "'.");
    }
  }

  const int m_in = relation_data.nrows();
  if (m_in <= 0) Rcpp::stop("relation_data has 0 rows.");

  const int n_pu = op->_n_pu;
  if (n_pu <= 0) Rcpp::stop("No planning units in model (op->_n_pu <= 0).");

  if (op->_w_offset < 0) Rcpp::stop("op->_w_offset not initialized.");
  if ((std::size_t)op->_w_offset + (std::size_t)n_pu > op->ncol_used()) {
    Rcpp::stop("w block out of bounds: check op->_w_offset/op->_n_pu vs number of variables.");
  }

  const int y0  = op->_y_pu_offset;
  const int n_y = op->_n_y_pu;

  if ((std::size_t)(y0 + n_y) > op->_obj.size()) {
    Rcpp::stop("y_pu block out of bounds relative to objective vector size.");
  }

  Rcpp::IntegerVector ip1 = relation_data["internal_pu1"];
  Rcpp::IntegerVector ip2 = relation_data["internal_pu2"];
  Rcpp::NumericVector  wgt = relation_data["weight"];

  bool directed = false;
  if (relation_data.containsElementNamed("directed")) {
    Rcpp::LogicalVector dir = relation_data["directed"];
    if (dir.size() != m_in) Rcpp::stop("relation_data$directed must have one value per row.");
    for (int r = 0; r < m_in; ++r) {
      if (dir[r] == NA_LOGICAL) Rcpp::stop("relation_data$directed must not contain NA.");
      if (r == 0) directed = static_cast<bool>(dir[r]);
      else if (static_cast<bool>(dir[r]) != directed) Rcpp::stop("A spatial relation cannot mix directed and undirected rows.");
    }
  }

  std::vector<double> linear_w(n_pu + 1, 0.0);
  std::unordered_map<std::uint64_t, double> edge_coef;
  int n_diag_rows = 0;

  for (int r = 0; r < m_in; ++r) {
    const int i1 = ip1[r];
    const int j1 = ip2[r];
    if (i1 == NA_INTEGER || j1 == NA_INTEGER) continue;
    if (i1 < 1 || i1 > n_pu || j1 < 1 || j1 > n_pu) Rcpp::stop("relation_data planning-unit index out of range.");
    const double we = static_cast<double>(wgt[r]);
    if (Rcpp::NumericVector::is_na(we) || !std::isfinite(we)) Rcpp::stop("relation_data weight must be finite.");
    if (i1 == j1) {
      //if (directed) Rcpp::stop("Directed spatial relations cannot contain self-arcs.");
      linear_w[i1] += we;
      ++n_diag_rows;
      continue;
    }
    if (we < 0.0) Rcpp::stop("relation_data off-diagonal weights must be >= 0.");
    int a = i1, b = j1;
    if (a > b) std::swap(a, b);
    const std::uint64_t key = edge_key_undirected(a, b);
    if (directed) {
      linear_w[i1] += we;
      edge_coef[key] -= we;
    } else {
      linear_w[i1] += we;
      linear_w[j1] += we;
      edge_coef[key] -= 2.0 * we;
    }
  }

  const int k_edges = static_cast<int>(edge_coef.size());
  if (k_edges != n_y) Rcpp::stop("Unique support-edge count does not match existing y_pu block size.");

  double sum_linear_added = 0.0;
  for (int i = 1; i <= n_pu; ++i) {
    const double coef = weight * weight_multiplier * linear_w[i];
    if (coef != 0.0) {
      op->_obj[static_cast<std::size_t>(op->_w_offset + i - 1)] += coef;
      sum_linear_added += coef;
    }
  }

  std::vector<std::pair<std::uint64_t, double>> edges(edge_coef.begin(), edge_coef.end());
  std::sort(edges.begin(), edges.end(), [](const auto& a, const auto& b){ return a.first < b.first; });
  double sum_edge_added = 0.0;
  for (int e = 0; e < k_edges; ++e) {
    const double coef = weight * weight_multiplier * edges[static_cast<std::size_t>(e)].second;
    op->_obj[static_cast<std::size_t>(y0 + e)] += coef;
    sum_edge_added += coef;
  }
  std::string full_tag = tag;
  if (!full_tag.empty()) full_tag += ";";
  full_tag +=
    "kind=objective_add"
    ";component=min_fragmentation"
    ";form=" + std::string(directed ? "directed_dependency" : "undirected_cut") +
      ";weight=" + std::to_string(weight) +
      ";multiplier=" + std::to_string(weight_multiplier) +
      ";m_in=" + std::to_string(m_in) +
      ";k_edges=" + std::to_string(k_edges) +
      ";n_diag_rows=" + std::to_string(n_diag_rows) +
      ";sum_edge_added=" + std::to_string(sum_edge_added) +
      ";sum_linear_added=" + std::to_string(sum_linear_added) +
      ";additive=TRUE";

  const std::size_t bid = op->register_objective_block(
    block_name,
    (std::size_t)0,
    op->ncol_used(),
    full_tag
  );

  return Rcpp::List::create(
    Rcpp::Named("ok") = true,
    Rcpp::Named("block_id") = (double)bid,
    Rcpp::Named("block_name") = block_name,
    Rcpp::Named("tag") = full_tag,
    Rcpp::Named("y_offset") = op->_y_pu_offset,
    Rcpp::Named("n_y") = op->_n_y_pu,
    Rcpp::Named("m_in") = m_in,
    Rcpp::Named("k_edges") = k_edges,
    Rcpp::Named("sum_edge_added") = sum_edge_added,
    Rcpp::Named("sum_linear_added") = sum_linear_added
  );
}
