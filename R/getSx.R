#' Get sx parameter
#'
#' ...
#' @param y Number of groups to include
#' @param Z value
#' @param k value
#' @return List of values needed to update parameters
#' The values sum to 1.
#' @keywords initiate gibbs inputs
#' @export
#' @examples
#'

getSx<-function(y, Z, k){
  # errors
  # 1. stop if k smaller than number of values in Z
  if(length(table(Z)) > k)  stop('number of distinct values in Z exceeds k')

  n     <- length(y)
  IndiZ <- suppressWarnings((Z == matrix((1:k), nrow = n, ncol = k, byrow = T)))
  sx    <- apply(IndiZ*y, 2, sum)
  return(sx)
}
