#' Update the allocation probabilities for multivariate Gaussian mixture
#'
#' ...
#' @param Mu Means
#' @param Cov input
#' @param Pi input
#' @param k input
#' @return Matrix of probabilities
#' @keywords multivariate Gaussian
#' @export
#' @examples
#'

loglike_UNIV<-function(z, y, Weights, Means, Variance){
    # reduce to occupied groups
    non0id<- table(z) %>% names() %>% as.numeric()
    Loglike =  0
    for (i in 1:length(y)){
    Loglike<-Loglike+ log(
      sum( Weights[non0id]*dnorm(y[i], mean=Means[non0id], sd=sqrt(Variance[non0id])))
      )
    }
    return(Loglike)
  }
