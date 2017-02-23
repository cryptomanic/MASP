#include <Rcpp.h>
using namespace Rcpp;
using namespace std;

// [[Rcpp::plugins(cpp11)]]
// [[Rcpp::export]]
List fitem_matrs(CharacterMatrix data) {
  List out(3);
  map<string, long long> ma;
  
  long long nrow = data.nrow();
  long long ncol = data.ncol();
  
  for(long long i = 0;i < nrow;i++) {
    for(long long j = 0;j < ncol;j++) {
      string temp = as<string>(data(i, j));
      ma[temp]++;
    }
  }
  
  long long cmax = 0;
  string smax;
  
  for(auto it = ma.begin();it != ma.end();it++) {
    if(it->second > cmax) {
      cmax = it->second;
      smax = it->first;
    }
  }
  
  CharacterMatrix block(cmax, ncol-1); long long brow = 0;
  CharacterMatrix cblock(nrow-cmax, ncol); long long cbrow = 0;
  
  for(long long i = 0;i < nrow;i++) {
    bool flag = false;
    for(long long j = 0;j < ncol;j++) {
      string temp = as<string>(data(i, j));
      if(temp == smax) {
        flag = true;
        break;
      }
    }
    
    if(flag) {
      long long k = 0;
      for(long long j = 0;j < ncol;j++) {
        string temp = as<string>(data(i, j));
        if(temp != smax)
          block(brow, k++) = data(i, j);
      }
      brow++;
    } else {
      for(long long j = 0;j < ncol;j++)
        cblock(cbrow, j) = data(i, j);
      cbrow++;
    }
  }
  
  return List::create(smax, block, cblock);
}

// helper function
// [[Rcpp::export]]
List uniqueItems(CharacterMatrix data) {
  map<string, int> ma;
  map<int, vector<string> > mivs;
  int n = data.nrow(), m = data.ncol();
  for(int i = 0;i < n;i++) 
    for(int j = 0;j < m;j++)
      if(ma.find(as<string>(data(i, j))) == ma.end()) ma[as<string>(data(i, j))] = i + 1;
  
  for(auto it = ma.begin();it != ma.end();it++) {
    if(mivs[it->second].size() == 0) mivs[it->second].push_back(to_string(it->second));
    mivs[it->second].push_back(it->first);
  }
  
  long long sz = mivs.size(), k = 0;
  List out(sz);
  for(auto it = mivs.begin(); it != mivs.end();it++) {
    out[k] = it->second;
    k++;
  }
  
  return out;
}

/*** R
set.seed(121)
sp <- c('a', 'b', 'c', 'd')
mat <- matrix(nrow = 5, ncol = 4)
mat[1, ] <- sample(sp)
mat[2, ] <- sample(sp)
mat[3, ] <- sample(sp)
mat[4, ] <- sample(sp)
mat[5, ] <- sample(sp)
fitem_matrs(mat)
*/
