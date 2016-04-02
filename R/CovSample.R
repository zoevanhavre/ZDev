#' Update the means (MULTIVARIATE)
#'
#' ...
#' @param covs Covariances
#' @param ns_mu input
#' @param ybar_mu input
#' @param n0 input
#' @param b0 input
#' @return Vector, draws from full conditional distribution of means given covariances,  priors, etc
#' The values sum to 1.
#' @keywords multivariate Gaussian
#' @export
#' @examples
#'

CovSample<-function(ns_cov, WkZ_cov, ybar_cov, c0, C0){
    c_cov<-c0+ns_cov
    if (ns_cov==0) {
      MCMCpack::riwish(c0, C0)
    } else {
      C_cov<- C0 +((ns_cov*n0)/(ns_cov+n0)*crossprod(ybar_cov-b0)) +WkZ_cov
      MCMCpack::riwish(c_cov,C_cov)
    }
  }
