#' GetYbar, MVN
#'
#' ...
#' @param Z allocations
#' @return List of values needed to update parameters
#' The values sum to 1.
#' @keywords initiate gibbs inputs
#' @export
#' @examples
#'

getYbar<-function(y, Z, k){
  n  <- dim(y)[1]
  ns <- getNK(Z, k)
  IndiZ <- (Z == matrix((1:k), nrow = n, ncol = k, byrow = T))
  .Ysplit <- replicate(k, list())	#storage to group Y's
  ybar    <- replicate(k, list(NA))

  for (.j in 1:k){
    .Ysplit[[.j]]<-y[Z==.j,]
    if (ns[.j]>1){					# for groups with >1 obsevations
      ybar[[.j]]<- as.matrix(t(apply(.Ysplit[[.j]], 2, mean)))
      } else if (ns[.j]==1){
      ybar[[.j]]<-  t( as.matrix(.Ysplit[[.j]]))
    }}
    return(ybar)
  }
