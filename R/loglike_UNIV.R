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

loglike_UNIV<-function(.Zs, y, Ps, Mus , Cov){
    non0id<- table(.Zs) %>% names() %>% as.numeric()
    Loglike =  0
    for (i in 1:length(y)){
    Loglike<-Loglike+ log(
      sum( Ps[non0id]*dnorm(y[i], mean=Mus[non0id], sd=sqrt(Cov[non0id])))
      )
    }
    return(Loglike)
  }
