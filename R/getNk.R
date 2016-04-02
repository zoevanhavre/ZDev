#' Count number of obs per group, including empty
#'
#' ...
#' @param Z allocations
#' @return vector
#' The values sum to n
#' @keywords count
#' @examples
#'

getNK<-function(Z,k){
  n <- length(Z)
  IndiZ <-  (Z == matrix((1:k), nrow = n, ncol = k, byrow = T))
  ns <-    IndiZ %>% apply(., 2, sum)	# size of each group, including empties
  return(ns)
}
