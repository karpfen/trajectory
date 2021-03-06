# Generated by using Rcpp::compileAttributes() -> do not edit by hand
# Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#' get_direction
#'
#' Calculates the linear directional mean of a given matrix with x and y
#' coordinates as colums.
#'
#' @param coords \code{arma::mat} object with x and y coordinates in the
#' columns.
#'
#' @return the linear directional mean.
#'
#' @noRd
NULL

#' rcpp_ldm
#'
#' Calculates the linear directional mean of a given set of trajectories.
#'
#' @param trj \code{Rcpp::List} where each entry contains a \code{matrix} of
#' coordinates.
#'
#' @return \code{Rcpp::NumericVector} containing the linear directional means.
#'
#' @noRd
rcpp_ldm <- function(trj) {
    .Call(`_trajectory_rcpp_ldm`, trj)
}

