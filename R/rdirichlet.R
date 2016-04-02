#' Draw from Dirichlet distribution
#'
#' This function draws samples from a dirichlet distriution with hyperpars m.
#' @param par Vector of inputs (Alpha+ number of obs in each group)
#' @param m=1 number of vectors to generate
#' @return Vector of probabilities, of same length as \code{par}.
#' The values sum to 1.
#' @keywords dirichlet
#' @export
#' @examples
#' rdirichlet(c(1,1,1))
#' rdirichlet(c(0.5,20,0.000000001))


rdirichlet<-function(par,m=1){
			k=length(par)         # for two mixture model k=2
			mat=matrix(0,m,k)       # makes an empty 1x2 matrix to store values
			for (i in 1:m)          # at this stage m is 1, when is it not 1?
			{
			sim=rgamma(k,shape=par,scale=1)
			mat[i,]=sim/sum(sim)      # simulated gamma scaled to sum to 1 and stored in matrix
			}
			mat               # matrix outputed
			}
