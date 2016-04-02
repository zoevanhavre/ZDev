#' Initiate the allocations for MCMC
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

initiate_values<-function(Z, y, k, r){
  Univ<-ifelse(is.vector(x), TRUE , FALSE)
  n<-ifelse(Univ, length(x), dim(x)[1])


  IndiZ <-  (Z == matrix((1:k), nrow = n, ncol = k, byrow = T))
  ns <-    IndiZ %>% apply(., 2, sum)	# size of each group, including empties

  .Ysplit<- replicate(k, list())	#storage to group Y's
  WkZ<-	    replicate(k, list(NA))	#storage for within group variability
  ybar<-	  replicate(k, list(NA))

  if(r==1){
  sx <- apply(IndiZ*y, 2, sum) } else{ sx<-NA}

  if(r>1){
    for (.i in 1:k){
      .Ysplit[[.i]]<-y[Z==.i,]
      if (ns[.i]>1){					# for groups with >1 obsevations
        ybar[[.i]]<- as.matrix(t(apply(.Ysplit[[.i]], 2, mean)))
        } else if (ns[.i]==1){
        ybar[[.i]]<-  t( as.matrix(.Ysplit[[.i]]))
      }
      #Within group unexplained variability
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
  }
        list('ns'=ns, 'sx'=sx, 'ybar'=ybar, 'WkZ'=WkZ, "IndiZ"=IndiZ)
  }
