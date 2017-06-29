#include <RcppArmadillo.h>
#include <math.h>

//' get_direction
//'
//' Calculates the linear directional mean of a given matrix with x and y
//' coordinates as colums.
//'
//' @param coords \code{arma::mat} object with x and y coordinates in the
//' columns.
//'
//' @return the linear directional mean.
//'
//' @noRd
float get_direction (arma::mat coords)
{
    arma::vec x = coords.col (0);
    arma::vec y = coords.col (1);
    arma::vec x0 = x.subvec (0, x.size () - 2);
    arma::vec y0 = y.subvec (0, y.size () - 2);
    arma::vec x1 = x.subvec (1, x.size () - 1);
    arma::vec y1 = y.subvec (1, y.size () - 1);

    arma::vec dx = x1 - x0;
    arma::vec dy = y1 - y0;
    arma::vec theta = atan2 (dy, dx);
    arma::vec sines = sin (theta);
    arma::vec cosines = cos (theta);
    float sum_sines = arma::accu (sines);
    float sum_cosines = arma::accu (cosines);
    float ldm = (atan2 (sum_sines, sum_cosines) / M_PI) * 180;
    if (ldm < 0)
        ldm += 360;
    return ldm;
}

//' rcpp_ldm
//'
//' Calculates the linear directional mean of a given set of trajectories.
//'
//' @param trj \code{Rcpp::List} where each entry contains a \code{matrix} of
//' coordinates.
//'
//' @return \code{Rcpp::NumericVector} containing the linear directional means.
//'
//' @noRd
// [[Rcpp::export]]
Rcpp::NumericVector rcpp_ldm (Rcpp::List trj)
{
    Rcpp::NumericVector directions (trj.size ());
    for (int i = 0; i < trj.size (); i ++)
    {
        arma::mat pts = trj [i];
        directions [i] = get_direction (pts);
    }
    return directions;
}
