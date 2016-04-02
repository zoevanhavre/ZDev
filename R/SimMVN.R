#' Simulate a multivariate Gaussian mixture
#'
#' ...
#' @param Means Means
#' @param Covs input
#' @param P weights
#' @param N input
#' @return list
#' @keywords multivariate Gaussian
#' @export
#' @examples
#'

SimMVN<-function(Means, Covs, P, N){
			TrueK<-length(P)
			r<-length(Means[[1]])
			Ys<-matrix(0,nrow = N, ncol = r)
			Zs<-matrix(0,nrow = N, ncol = 1)
			for (.n in 1:N){
			#draw Z according to probability
			.z<-sample(c(1:TrueK), size=1, prob=P)
			Zs[.n,]<-.z
			#generate random sample
			Ys[.n,]<-mvtnorm::rmvnorm(1, Means[[.z]], Covs[[.z]])
			}
			list(Y=Ys, Z=Zs)}
