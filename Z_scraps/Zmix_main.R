#' Main function for Gibbs sampler with Tempering
#'
#' ...
#' @param y data
#' @param k value
#' @return List of values needed to update parameters
#' The values sum to 1.
#' @keywords initiate gibbs inputs
#' @export
#' @examples
#'


Zmix_main<-function(
          y,
          iterations = 2000,
          k = 10,
          alphas = c(1/2^(c(6, 10, 30))),
          tau = 0.1,
          burn = 500,
          init.method = "Kmeans",
          verbose = TRUE
          ){


### STEP 1, compute unchanging values

  # Is it Univarate or Multivariate?
  r = ifelse(is.vector(y),  1, dim(y)[2])

# test
if(verbose){ paste("The data contains", r, "dimension(s)")%>%print() }

  ## Univarate
  if (r==1){
    n      = length(y)
    a      = 2.5
    b      = 2/var(y)
    lambda = sum(y)/n
    d      = sum(c(1:r))+r
    mux    = list( mu=seq(from=min(y), to=max(y),length.out=k),
              sigma=rep(1, k), p=rep(1/k,k), k=k)  # CHANGE THIS? so start is with allocs
    mu0    = mux$mu
    p0     = mux$p
    sig0   = mux$sigma
  } else {
  ## Multivariate
    r  =  dim(y)[2]
    n  =  dim(y)[1]
    n0 =  1
    Ck =	replicate(k, list())
    c0 =  r+1
    b0 =  apply(y,2,mean)
    C0 =  0.75*cov(y)
    d  =  sum(c(1:r))+r
  }

  # Number of chains for tempering
    nCh =     length(alphas)

# STORAGE: structure is parameter[[chain]][[iterations]][[k]]
 Ps<-     replicate(nCh, replicate(iterations, list()))
 Mus<-    replicate(nCh, replicate(iterations, list()))
 Covs<-   replicate(nCh, replicate(iterations, list()))
 Zs<-     replicate(nCh, replicate(iterations, list()))

 for (.it in 1:iterations){  #for each iteration
   for (.ch in 1:nCh){


### Update inputs needed
     if (.it==1){
        # iteration=1, initiallize input values
        } else {
        # Update given current pars
        }

   } # iterations

 } # chains



}
