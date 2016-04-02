#' Update the allocation probabilities for multivariate Gaussian mixture
#'
#' ...
#' @param Mu Means
#' @param Cov input
#' @param Pi input
#' @param k input
#' @return Matrix of probabilities
#' @keywords multivariate Gaussian
#' @export
#' @examples
#'

Update_probs<-function(Mu, Cov, Pi, k ){
    update_group_Prob <-function(x){
        a1<-dmvn(y, Mu[[x]], Cov[[x]])
        a2<-a1*Pi[x]
        return(a2)
      }
    # apply to each component
    ugp<-mapply(update_group_Prob, c(1:k))
    # scale each row
    ugp/apply(ugp, 1, sum)
  }
