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



getSV<-function(.mus, n, y, IndiZ){
  .bmu <- rep(.mus, n) %>%  matrix(. , ncol =n) %>% t()
  apply((y*IndiZ-.bmu*IndiZ)^2, 2, sum)
  }
