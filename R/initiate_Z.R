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


initiate_Z<-function( x, k, method=c("Kmeans", "random", "single", "uniform")){
  # get n
  Univ<-ifelse(is.vector(x), TRUE , FALSE)
  n<-ifelse(Univ, length(x), dim(x)[1])

  if(method=="Kmeans"){
  init.allocations<-  as.vector(kmeans(x, centers=k)$cluster)
  } else if (method=="random"){
    init.allocations<-  base::sample(c(1:k), n, replace=TRUE)
  } else if (method=="single"){
    init.allocations<-  rep(as.integer(1), n)
  } else if (method=="uniform"){
    init.allocations<-c()
    for (i in 1:k) {
      init.allocations <- c(init.allocations, rep(i, ceiling(n/k) ))}
    init.allocations<-init.allocations[1:n]
  }
  return(init.allocations)
}
