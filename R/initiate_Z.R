#' Initiate the allocations for MCMC
#'
#' ...
#' @param x data
#' @param k Number of groups to include
#' @param n value
#' @param method Type of initiallization: Kmeans, random, or single
#' @param m=1 number of vectors to generate
#' @return Vector of inital allocations for starting mcmc
#' The values sum to 1.
#' @keywords dirichlet
#' @export
#' @examples
#'


initiate_Z<-function( x, k, n, method=c("Kmeans", "random", "single")){
  if(method=="Kmeans"){
  init.allocations<-  as.vector(kmeans(x, centers=k)$cluster)
  } else if (method=="random"){
    init.allocations<-  base::sample(c(1:k), n, replace=TRUE)
  } else if (method=="single"){
    init.allocations<-  rep(1, n)
  }
  return(init.allocations)
}
