#' Density of Dirichlet distribution Function
#'
#' density of Dirichlet
#' @param x Input, must sum to 1
#' @param alpha Hyper-parameter
#' @keywords dirichlet
#' @export
#' @examples dDirichlet(c(.1, .9), c(0.1,0.1))



dDirichlet<-function (x, alpha) {
           dlog = lgamma(sum(alpha)) + sum((alpha - 1) * log(x)) - sum(lgamma(alpha))
           result =exp(dlog)
           return(result)
         }
