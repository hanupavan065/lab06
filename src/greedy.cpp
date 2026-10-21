#include <Rcpp.h>
#include <algorithm>
#include <numeric>
#include <vector>

// [[Rcpp::export]]
Rcpp::IntegerVector greedy_cpp(
    Rcpp::NumericVector w,
    Rcpp::NumericVector v,
    double W) {
  
  int n = w.size();
  
  std::vector<int> ord(n);
  std::iota(ord.begin(), ord.end(), 0);
  
  std::vector<double> ratio(n);
  
  for (int i = 0; i < n; ++i) {
    ratio[i] = v[i] / w[i];
  }
  
  std::sort(ord.begin(), ord.end(),
            [&](int a, int b) {
              if (ratio[a] != ratio[b]) {
                return ratio[a] > ratio[b];
              }
              
              if (v[a] != v[b]) {
                return v[a] > v[b];
              }
              
              return a < b;
            });
  
  std::vector<int> selected;
  
  double used = 0.0;
  double selected_value = 0.0;
  
  for (int id : ord) {
    
    if (w[id] <= W - used) {
      
      selected.push_back(id + 1);
      
      used += w[id];
      selected_value += v[id];
    }
  }
  
  int best_single = -1;
  
  for (int i = 0; i < n; ++i) {
    
    if (w[i] <= W &&
        (best_single < 0 ||
        v[i] > v[best_single])) {
      
      best_single = i;
    }
  }
  
  if (best_single >= 0 &&
      v[best_single] > selected_value) {
    
    return Rcpp::IntegerVector::create(
      best_single + 1
    );
  }
  
  return Rcpp::wrap(selected);
}
