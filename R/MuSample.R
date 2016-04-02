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
MuSample<-function(covs, ns_mu, ybar_mu, n0, b0 ){
    if (ns_mu==0) {
      rmvn(1, b0, covs/n0)
      } else {
        bk<-(n0/(ns_mu+n0))*b0+(ns_mu/(n0+ns_mu))*ybar_mu
        Bk<-(1/(ns_mu+n0))*covs
        rmvn(1, t(bk), Bk)
      }
  }
