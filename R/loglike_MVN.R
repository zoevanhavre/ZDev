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

loglike_MVN<-function(.Zs,y,Ps,Mus , Cov){
    non0id<-.Zs %>% factor() %>% levels() %>% as.numeric()
    Loglike<-0
    for (i in 1:length(.Zs)){
      .ll<-0
      for (numK in 1:length(non0id)){
         .ll<-.ll+
         Ps[non0id[numK]]*dmvn(
           y[i,], Mus[[ non0id[numK] ]],
           Cov[[ non0id[numK] ]]) }
      Loglike<-Loglike + log(.ll)
    }
   return(Loglike)
  }
