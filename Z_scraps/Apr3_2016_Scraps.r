# Notes and bits

git init
git remote add origin https://github.com/zoevanhavre/ZDev.git
git pull origin master
git push origin master

# workflow
git add -A
git commit -m 'commit description'
git push


## building package
  install.packages(c("devtools", "roxygen2", "testthat", "knitr"))

  devtools::use_testthat()

  # devtools::create("/Users/zoevanhavre/Documents/Zdev")
  ## add required package
  # devtools::use_package("dplyr")
  devtools::use_package("mvnfast")



  # Update documentation
  devtools::document()

  # update and build
  devtools::test()
  devtools::document(); devtools::build()
# WORKSPACE
  # y for testin
  yu<-cbind(rnorm(20, 1, 1), rnorm(30, 3, 2))
  yu<-yu[sample(1:length(yu), length(yu))] # randomize

  # mvn
  ym<-SimMVN(     Means = list( c(3,5), c(1,5)),
                  Covs = list( matrix(c(10,0.1,0.1,0.5), nrow=2),
                  matrix(c(0.5,0.1,0.1,10), nrow=2)),
                  P=c(0.7,0.3),
                  N=50)$Y



########
# mvn workflow inside function

y<-ym





# do i use tibbles?
library(dplyr)
library(tibble)




library("mvtnorm")
library("dplyr")
library("compiler")
library("microbenchmark")
library("ggplot2")
library("mvnfast")
library("Zmix")

Zmix_main<-function(
          y,
          iterations=2000,
          k=10,
          alphas= c(1/2^(c(6, 10, 30))),
          tau=1,
          burn=500,
          init.method="Kmeans",
          verbose=TRUE
          ){
# compute basic values
  nCh<- length(alphas)

  if(is.vector(y)){
    # UNIVARIATE
    r<-1
    n<-length(y)
    ## univ hyper priors
    a<-       2.5
    b<-       2/var(y)
    lambda<-  sum(y)/n
    d<-   sum(c(1:r))+r
    mux<-list(mu=seq(from=min(y), to=max(y),length.out=k),sigma=rep(1, k),p=rep(1/k,k), k=k)
    mu0<- mux$mu
    p0<-  mux$p
    sig0<-mux$sigma
  } else {
    # MULTIVARIATE
    r<-   dim(y)[2]
    n<-   dim(y)[1]
    n0<-  1
    Ck<-	replicate(k, list())
    c0<-  r+1
    b0<-  apply(y,2,mean)
    C0<-  0.75*cov(y)
    d<-   sum(c(1:r))+r
  }

  Loglike <-   rep(0,iterations)
  SteadyScore<-data.frame("Iteration"=c(1:iterations), "K0"=0)
  map <-    matrix(0,nrow = iterations, ncol = 1)

## parameters to estimate
## storage structure  par[[chain]][[iterations]][[k]]
 Ps<-     replicate(nCh, replicate(iterations, list()))
 Mus<-    replicate(nCh, replicate(iterations, list()))
 Covs<-   replicate(nCh, replicate(iterations, list()))
 Zs<-     replicate(nCh, replicate(iterations, list()))

 if(verbose==TRUE){pb <- txtProgressBar(min = 0, max = iterations, style = 3)}

  # FOR EACH ITERATION
  for (.it in 1:iterations){  #for each iteration

    # TRACKER
      if(verbose==TRUE && .it %% 10 == 0) {
        Sys.sleep(0.01)
        if(r>1){
        par(mfrow=c(2,1))
        plot(SteadyScore$K0~SteadyScore$Iteration, main='#non-empty groups', type='l')
        ts.plot( t(sapply(Ps[[nCh]], rbind)), main='Target Weights', col=rainbow(k))
        }
        Sys.sleep(0)
        setTxtProgressBar(pb, .it)
       }

  # FOR EACH CHAIN
  for (.ch in 1:nCh){
    # Input .values
     if (.it==1){
        # iteration=1, initiallize input values
        init.Z<-          initiate_Z(y,k,n, init.method)
        input.values<-    initiate_values(init.Z, y,n, k,r)
          ns<-  input.values$ns
          ybar<-input.values$ybar
          WkZ<- input.values$WkZ
          sx<-  input.values$sx
          IndiZ<-input.values$IndiZ
        } else {
        # Update given current pars
        input.values<-initiate_values(Zs[[.ch]][[.it-1]] ,y,n, k,r)
          ns<-    input.values$ns
          ybar<-  input.values$ybar
          WkZ<-   input.values$WkZ
          sx<-    input.values$sx
          IndiZ<-input.values$IndiZ
        }

      # STEP 2.1 : GENERAnTE Samples for WEIGHTS from DIRICHLET dist
      Ps[[.ch]][[.it]] <- rdirichlet(m=1, par= ns+alphas[.ch])

        if (r>1){
        # STEP 2.2 GENERATE Samples from Covariance
        Covs[[.ch]][[.it]]<-mapply(CovSample, ns, WkZ, ybar, rep(list(c0), k), rep(list(C0), k),SIMPLIFY=FALSE)

        # STEP 2.3 GENERATE SAMPLEs of Means
        Mus[[.ch]][[.it]]<- mapply(MuSample,Covs[[.ch]][[.it]], ns, ybar, rep(list(n0), k), rep(list(b0), k), SIMPLIFY=FALSE)

        # STEP 3.1: Draw new classification probabilities:
        PZs<-Update_probs(Mus[[.ch]][[.it]], Covs[[.ch]][[.it]], Ps[[.ch]][[.it]],k)

        # STEP 3.2: Update allocations based on probabilitie
        Zs[[.ch]][[.it]]<-Update_Zs(PZs)
        }

      # r=1
      if (r==1){
    # MEANS
        if (.it==1){
          Mus[[.ch]][[.it]]<-  rnorm(k,	mean=(lambda*tau+sx)/(tau+ns), sd=sqrt(sig0/(tau+ns)))
        } else {
          Mus[[.ch]][[.it]]<-  rnorm(k,	mean=(lambda*tau+sx)/(tau+ns), sd=sqrt(Covs[[.ch]][[.it-1]]/(tau+ns)))
        }

    # Variance
        for (i in 1:k){if (is.na(Mus[[.ch]][[.it]][i])){ Mus[[.ch]][[.it]][i]<-0 }}
        sv<-getSV(Mus[[.ch]][[.it]],n, y, IndiZ)
      Covs[[.ch]][[.it]]<- rinvgamma(k, a+(ns+1)/2,	b+0.5*tau*(Mus[[.ch]][[.it]]-lambda)^2+0.5*sv)
     }


## PRIOR PARALLEL TEMPERING
if(.it>20 && runif(1)<.9){
  Chain1<-sample( 1:(nCh-1), 1) ; 	Chain2<-Chain1+1
  MHratio<- parallelAccept(Ps[[Chain1]][[.it]], Ps[[Chain2]][[.it]], rep(alphas[Chain1],k), rep(alphas[Chain2],k))
  if (MHratio==1){
  # Flip the allocations
  .z1<-	Zs[[Chain1]][[.it]] 	;.z2<-	Zs[[Chain2]][[.it]]
  Zs[[Chain1]][[.it]]<-.z2 	;Zs[[Chain2]][[.it]]<-.z1

  #Mu
  .mu1<-	Mus[[Chain1]][[.it]] 	;.mu2<-	Mus[[Chain2]][[.it]]
  Mus[[Chain1]][[.it]]<-.mu2 	;Mus[[Chain2]][[.it]]<-.mu1

  #Cov
  .cv1<-	Covs[[Chain1]][[.it]] 	;.cv2<-	Covs[[Chain2]][[.it]]
  Covs[[Chain1]][[.it]]<-.cv2 	;Covs[[Chain2]][[.it]]<-.cv1

  #Ps
  .p1<-	Ps[[Chain1]][[.it]] 	;.p2<-	Ps[[Chain2]][[.it]]
  Ps[[Chain1]][[.it]]<-.p2 	;Ps[[Chain2]][[.it]]<-.p1
  }
}
}


}
# end iteration loop

# compute log like
for (iter in 1:iterations){
  Loglike[iter]<-ifelse(r>1,
  loglike_MVN(Zs[[nCh]][[iter]], y,
    Ps[[nCh]][[iter]], Mus[[nCh]][[iter]], Covs[[nCh]][[iter]] ),
  loglike_UNIV(Zs[[nCh]][[iter]], y,
     Ps[[nCh]][[iter]], Mus[[nCh]][[iter]], Covs[[nCh]][[iter]])
     )
   }
  if(verbose==TRUE){close(pb)}

  burn<-burn+1
	nCh                                     #number of chains
	Mu.burned<-Mus[[nCh]][burn:iterations]
	Cov.burned<-Covs[[nCh]][burn:iterations]
	Ps.burned<-Ps[[nCh]][burn:iterations]
	Zs.burned<-Zs[[nCh]][burn:iterations]
  Loglike.burned<-Loglike[burn:iterations]
	SteadyScore.burned<-SteadyScore$K0[burn:iterations]

# make weights and Zs matrices
Zs.burned<-t(sapply(Zs.burned, rbind))
Ps.burned<-t(sapply(Ps.burned, cbind))

# if r=1, make mu and var a matrix too
if (r==1){
  Mu.burned<-t(sapply(Mu.burned, rbind))
  Cov.burned<-t(sapply(Cov.burned, rbind))
}
	return(
    list(
    "Mu" = Mu.burned,
    "Cov"=Cov.burned,
    "Ps"= Ps.burned,
    "Zs"=Zs.burned,
    "k.occupied"=SteadyScore.burned,
    "Log.likelihood"=Loglike,
    "y"=y))
}
