#' Update / get sv
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


getSV<-function(Means, y, z){
  n     <- length(y)
  IndiZ <- suppressWarnings((z == matrix((1:k), nrow = n, ncol = k, byrow = T)))
  .bmu <- rep(Means, n) %>%  matrix(. , ncol =n) %>% t()
  apply((y*IndiZ-.bmu*IndiZ)^2, 2, sum)
  }
