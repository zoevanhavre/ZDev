#' Update the Allocations
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

Update_Zs<-function(PZs){
  k<-dim(PZs)[2]
  n<-dim(PZs)[1]
  mapply( function(x) sample(c(1:k), 1, prob=PZs[x,]), c(1:n))
}
