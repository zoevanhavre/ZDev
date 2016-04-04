#' Initiate the values for MCMC (UNIVARIATE)
#'
#' ...
#' @param y data, as a vector
#' @param k Number of groups in model
#' @param init.method Method for choosing initial annotations, "Kmeans"  fits a k component Kmeans to the data, while "uniform" spreads k groups evenly across the range of the data.
#' @return List of values needed to start MCMC.
#' The first element is a tibble containing mu0, sig0, and p0. The second, a vector of integers containing the allocations init.Z.
#' @keywords initiate gibbs inputs
#' @export
#' @examples
#'  y <- rnorm(10,1, 10)
#'  initiate_univ(y, k=2, init.method="uniform")
#'  initiate_univ(y, k=5, init.method="uniform")
#'  initiate_univ(y, k=5, init.method="Kmeans")



initiate_univ<-function(y, k, init.method=c("Kmeans", "uniform")){
init.Z<-initiate_Z(y, k, init.method)
init_yz<-tibble::data_frame(y, init.Z)
# group
 grouped <- dplyr::group_by(init_yz, init.Z)
.sums<- dplyr::summarise(grouped, mu0=mean(y), sig0=sd(y), p0=length(init.Z))
.sums["p0"] <- .sums["p0"]/length(y)

return(list(.sums, init.Z))
}
