#' Within group unexplained variability
#'
#' ...
#' @param Z allocations
#' @param y Number of groups to include
#' @param n value
#' @param k value
#' @param r value
#' @return List of values needed to update parameters
#' The values sum to 1.
#' @keywords initiate gibbs inputs
#' @export
#' @examples
#'

getWkZ<-function(y, Z, k){
#Within group unexplained variability
  n  <-       dim(y)[1]
  ns <-       getNK(Z, k)
  ybar <- getYbar(y,Z,k)
  IndiZ <-    (Z == matrix((1:k), nrow = n, ncol = k, byrow = T))
  .Ysplit <-  replicate(k, list())	#storage to group Y's
  WkZ <-      replicate(k, list(NA))	#storage for within group variability
  for (.i in 1:k){
    .Ysplit[[.i]]<-y[Z==.i,]
    if (ns[.i]==1){
      WkZ[[.i]]<-crossprod(as.matrix(.Ysplit[[.i]]-ybar[[.i]]))
    } else if (ns[.i]>1){
      WkZ[[.i]]<-0
      for (.n in 1:ns[.i]){
        WkZ[[.i]]<-WkZ[[.i]]+ crossprod( .Ysplit[[.i]][.n,]-ybar[[.i]])
      }
    } else if (ns[.i]==0){
        WkZ[[.i]]<-NA
        }
      }
  return(WkZ)
}
