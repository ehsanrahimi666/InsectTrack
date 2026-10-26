#include <Rcpp.h>
#include <algorithm>
#include <cmath>
#include <vector>
using namespace Rcpp;

// Box mean filter with a square window of half-width r (clipped at borders),
// computed from an integral image.
// [[Rcpp::export]]
NumericMatrix it_box_mean(NumericMatrix x, int r) {
  const int nr = x.nrow(), nc = x.ncol();
  if (r <= 0) return clone(x);
  std::vector<double> S((size_t)(nr + 1) * (nc + 1), 0.0);
  auto at = [nr](int i, int j) { return (size_t)j * (nr + 1) + i; };
  for (int j = 0; j < nc; ++j) {
    double colsum = 0.0;
    for (int i = 0; i < nr; ++i) {
      colsum += x(i, j);
      S[at(i + 1, j + 1)] = S[at(i + 1, j)] + colsum;
    }
  }
  NumericMatrix out(nr, nc);
  for (int j = 0; j < nc; ++j) {
    const int j0 = std::max(0, j - r), j1 = std::min(nc - 1, j + r);
    for (int i = 0; i < nr; ++i) {
      const int i0 = std::max(0, i - r), i1 = std::min(nr - 1, i + r);
      const double s = S[at(i1 + 1, j1 + 1)] - S[at(i0, j1 + 1)] -
                       S[at(i1 + 1, j0)] + S[at(i0, j0)];
      out(i, j) = s / ((double)(i1 - i0 + 1) * (double)(j1 - j0 + 1));
    }
  }
  return out;
}

// Binary dilation (dilate = true) or erosion with a square structuring
// element of half-width r.
// [[Rcpp::export]]
LogicalMatrix it_morph(LogicalMatrix m, int r, bool dilate) {
  const int nr = m.nrow(), nc = m.ncol();
  if (r <= 0) return clone(m);
  std::vector<int> S((size_t)(nr + 1) * (nc + 1), 0);
  auto at = [nr](int i, int j) { return (size_t)j * (nr + 1) + i; };
  for (int j = 0; j < nc; ++j) {
    int colsum = 0;
    for (int i = 0; i < nr; ++i) {
      colsum += (m(i, j) == TRUE);
      S[at(i + 1, j + 1)] = S[at(i + 1, j)] + colsum;
    }
  }
  LogicalMatrix out(nr, nc);
  for (int j = 0; j < nc; ++j) {
    const int j0 = std::max(0, j - r), j1 = std::min(nc - 1, j + r);
    for (int i = 0; i < nr; ++i) {
      const int i0 = std::max(0, i - r), i1 = std::min(nr - 1, i + r);
      const int s = S[at(i1 + 1, j1 + 1)] - S[at(i0, j1 + 1)] -
                    S[at(i1 + 1, j0)] + S[at(i0, j0)];
      const int area = (i1 - i0 + 1) * (j1 - j0 + 1);
      out(i, j) = dilate ? (s > 0) : (s == area);
    }
  }
  return out;
}

// Per-pixel median of a stack of frames stored as an array of dimension
// npix x nfr (column-major, frames last).
// [[Rcpp::export]]
NumericVector it_median_stack(NumericVector a, int npix, int nfr) {
  if ((R_xlen_t)npix * nfr != a.size()) stop("array size does not match npix * nfr");
  NumericVector out(npix);
  std::vector<double> v(nfr);
  const size_t mid = nfr / 2;
  for (int p = 0; p < npix; ++p) {
    for (int k = 0; k < nfr; ++k) v[k] = a[p + (R_xlen_t)npix * k];
    std::nth_element(v.begin(), v.begin() + mid, v.end());
    double med = v[mid];
    if (nfr % 2 == 0) {
      const double lo = *std::max_element(v.begin(), v.begin() + mid);
      med = 0.5 * (med + lo);
    }
    out[p] = med;
  }
  return out;
}

// Per-pixel median and spread of a stack of frames stored as npix x nfr.
// The spread is the 75th percentile of the absolute deviations from the
// median divided by 1.1503 (equal to the standard deviation for normal data);
// insects present in fewer than a quarter of the frames do not inflate it.
// [[Rcpp::export]]
List it_median_spread_stack(NumericVector a, int npix, int nfr) {
  if ((R_xlen_t)npix * nfr != a.size()) stop("array size does not match npix * nfr");
  NumericVector med(npix), spr(npix);
  std::vector<double> v(nfr), dv(nfr);
  const size_t mid = nfr / 2;
  const size_t q75 = (size_t)std::floor(0.75 * (nfr - 1) + 0.5);
  for (int p = 0; p < npix; ++p) {
    for (int k = 0; k < nfr; ++k) v[k] = a[p + (R_xlen_t)npix * k];
    std::nth_element(v.begin(), v.begin() + mid, v.end());
    double m = v[mid];
    if (nfr % 2 == 0) m = 0.5 * (m + *std::max_element(v.begin(), v.begin() + mid));
    for (int k = 0; k < nfr; ++k) dv[k] = std::fabs(a[p + (R_xlen_t)npix * k] - m);
    std::nth_element(dv.begin(), dv.begin() + q75, dv.end());
    med[p] = m;
    spr[p] = dv[q75] / 1.1503;
  }
  return List::create(_["median"] = med, _["spread"] = spr);
}

// Connected components (8-connectivity) of a binary mask with blob
// statistics. Coordinates are pixel centres in image units: x = column - 0.5,
// y = row - 0.5 (origin at the top-left corner of the image). The weight
// matrix (e.g. absolute difference from the background) gives the
// intensity-weighted centroid.
// [[Rcpp::export]]
DataFrame it_label_blobs(LogicalMatrix m, NumericMatrix w, int min_area,
                         double max_area) {
  const int nr = m.nrow(), nc = m.ncol();
  if (w.nrow() != nr || w.ncol() != nc) stop("mask and weight differ in size");
  std::vector<int> lab((size_t)nr * nc, 0);
  std::vector<int> parent(1, 0);
  auto find = [&parent](int a) {
    while (parent[a] != a) {
      parent[a] = parent[parent[a]];
      a = parent[a];
    }
    return a;
  };
  auto unite = [&](int a, int b) {
    a = find(a);
    b = find(b);
    if (a != b) {
      if (a < b) parent[b] = a; else parent[a] = b;
    }
  };
  auto L = [&lab, nr](int i, int j) -> int& { return lab[(size_t)j * nr + i]; };
  int next = 1;
  for (int j = 0; j < nc; ++j) {
    for (int i = 0; i < nr; ++i) {
      if (m(i, j) != TRUE) continue;
      int nb[4];
      int k = 0;
      if (i > 0 && L(i - 1, j)) nb[k++] = L(i - 1, j);
      if (j > 0) {
        if (i > 0 && L(i - 1, j - 1)) nb[k++] = L(i - 1, j - 1);
        if (L(i, j - 1)) nb[k++] = L(i, j - 1);
        if (i < nr - 1 && L(i + 1, j - 1)) nb[k++] = L(i + 1, j - 1);
      }
      if (k == 0) {
        parent.push_back(next);
        L(i, j) = next++;
      } else {
        int mn = nb[0];
        for (int q = 1; q < k; ++q) mn = std::min(mn, nb[q]);
        L(i, j) = mn;
        for (int q = 0; q < k; ++q) unite(mn, nb[q]);
      }
    }
  }
  const int nl = next;
  std::vector<int> root(nl, 0), id(nl, -1);
  int nb = 0;
  for (int a = 1; a < nl; ++a) {
    root[a] = find(a);
    if (id[root[a]] < 0) id[root[a]] = nb++;
  }
  std::vector<double> area(nb, 0), sx(nb, 0), sy(nb, 0), sxx(nb, 0), syy(nb, 0),
      sxy(nb, 0), sw(nb, 0), swx(nb, 0), swy(nb, 0), mw(nb, 0);
  std::vector<double> x0(nb, R_PosInf), x1(nb, R_NegInf), y0(nb, R_PosInf),
      y1(nb, R_NegInf);
  for (int j = 0; j < nc; ++j) {
    for (int i = 0; i < nr; ++i) {
      const int l = L(i, j);
      if (!l) continue;
      const int b = id[root[l]];
      const double x = j + 0.5, y = i + 0.5, ww = w(i, j);
      area[b] += 1;
      sx[b] += x; sy[b] += y;
      sxx[b] += x * x; syy[b] += y * y; sxy[b] += x * y;
      sw[b] += ww; swx[b] += ww * x; swy[b] += ww * y;
      mw[b] = std::max(mw[b], ww);
      x0[b] = std::min(x0[b], (double)j); x1[b] = std::max(x1[b], (double)j + 1);
      y0[b] = std::min(y0[b], (double)i); y1[b] = std::max(y1[b], (double)i + 1);
    }
  }
  std::vector<int> keep;
  for (int b = 0; b < nb; ++b)
    if (area[b] >= min_area && area[b] <= max_area) keep.push_back(b);
  const int n = keep.size();
  NumericVector X(n), Y(n), A(n), XC(n), YC(n), XMIN(n), XMAX(n), YMIN(n),
      YMAX(n), MEANW(n), MAXW(n), ORI(n), MAJ(n), MIN(n);
  for (int q = 0; q < n; ++q) {
    const int b = keep[q];
    const double a = area[b];
    const double cx = sx[b] / a, cy = sy[b] / a;
    const double mu20 = sxx[b] / a - cx * cx, mu02 = syy[b] / a - cy * cy,
                 mu11 = sxy[b] / a - cx * cy;
    const double tr = 0.5 * (mu20 + mu02);
    const double dd = std::sqrt(std::max(0.0, 0.25 * (mu20 - mu02) * (mu20 - mu02) + mu11 * mu11));
    A[q] = a;
    XC[q] = cx; YC[q] = cy;
    X[q] = sw[b] > 0 ? swx[b] / sw[b] : cx;
    Y[q] = sw[b] > 0 ? swy[b] / sw[b] : cy;
    XMIN[q] = x0[b]; XMAX[q] = x1[b]; YMIN[q] = y0[b]; YMAX[q] = y1[b];
    MEANW[q] = sw[b] / a; MAXW[q] = mw[b];
    ORI[q] = 0.5 * std::atan2(2.0 * mu11, mu20 - mu02);
    MAJ[q] = 4.0 * std::sqrt(std::max(0.0, tr + dd));
    MIN[q] = 4.0 * std::sqrt(std::max(0.0, tr - dd));
  }
  return DataFrame::create(_["x"] = X, _["y"] = Y, _["area"] = A,
                           _["x_centroid"] = XC, _["y_centroid"] = YC,
                           _["xmin"] = XMIN, _["xmax"] = XMAX,
                           _["ymin"] = YMIN, _["ymax"] = YMAX,
                           _["contrast_mean"] = MEANW, _["contrast_max"] = MAXW,
                           _["orientation"] = ORI, _["major_axis"] = MAJ,
                           _["minor_axis"] = MIN);
}

//' Points inside a polygon
//'
//' Even-odd rule test for many points and one polygon.
//'
//' @param px,py Point coordinates.
//' @param vx,vy Polygon vertices (the polygon is closed automatically).
//' @return Logical vector (`NA` for points with missing coordinates).
//' @export
// [[Rcpp::export]]
LogicalVector it_point_in_polygon(NumericVector px, NumericVector py,
                                  NumericVector vx, NumericVector vy) {
  const int n = px.size(), m = vx.size();
  LogicalVector out(n);
  for (int k = 0; k < n; ++k) {
    bool in = false;
    if (NumericVector::is_na(px[k]) || NumericVector::is_na(py[k])) {
      out[k] = NA_LOGICAL;
      continue;
    }
    for (int i = 0, j = m - 1; i < m; j = i++) {
      if (((vy[i] > py[k]) != (vy[j] > py[k])) &&
          (px[k] < (vx[j] - vx[i]) * (py[k] - vy[i]) / (vy[j] - vy[i]) + vx[i]))
        in = !in;
    }
    out[k] = in;
  }
  return out;
}
