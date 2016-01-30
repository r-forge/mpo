library(quadprog)
library(Rglpk)

# Constraints Object for solve.QP

constraints = function(A,b,meq)
{
    list(A=A,b=b,meq=meq)
}

# GMV Portfolio with Constraints
# If cset = NULL, then unconstrained gmv
# For back-test use default wts.only = T
# For efficient frontier use wts.only = Fgm
# For  printout of wts, mu and sd use digits = 3 or 4
require("corpcor")

gmv = function(returns,cset=NULL,wts.only=T,digits = NULL)
{
   returns.old <- returns
   
  if (any(c("turnover","propcost") %in% cset$clist.names)){
      returns <- cbind(returns,returns,returns)      
    } 
	
	makenullmat <- function(A){matrix(0,nrow=nrow(A),ncol=ncol(A))}
	
  if ("turnover.doug" %in% cset$clist.names){
		returns <- cbind(returns,makenullmat(returns),makenullmat(returns))      
	} 
	
    cov.mat <- cov(returns)
    Dmat <- 2*cov.mat
    #Make covariance positive definite
    #This should barely change the covariance matrix, as
    #the last few eigen values are very small negative numbers
    Dmat <- make.positive.definite(Dmat)
    mu <- apply(returns,2,mean)
     #no linear part in this problem
    p = ncol(returns)
    if(is.null(cset))
    {A = matrix(rep(1,p),ncol =1)
    b = 1
    meq = 1}     else
    {A = cset$A
    b = cset$b
    meq = cset$meq}
    dvec <- rep(0,nrow(A))
   
    port.gmv = solve.QP(Dmat,dvec,A,b,meq)
    wts = port.gmv$solution      # Get optimal weights
    
    if (any(c("turnover","propcost") %in% cset$clist.names)){
      wts = wts[1:(p/3)]+wts[(p/3+1):(p/3*2)]+wts[(p/3*2+1):p]
      mu = sum(wts*mu[1:(p/3)])
      wts = as.matrix(wts)
      sd = as.numeric(sqrt((t(wts)%*%cov(returns.old)%*%wts)))
      wts = as.numeric(wts)
      names(wts)= dimnames(returns.old)[[2]]
    } 
	else if("turnover.doug" %in% cset$clist.names){
		wts = wts[1:(p/3)]
		mu = sum(wts*mu[1:(p/3)])
		wts = as.matrix(wts)
		sd = as.numeric(sqrt((t(wts)%*%cov(returns.old)%*%wts)))
		wts = as.numeric(wts)
		names(wts)= dimnames(returns.old)[[2]]
	}  
	else{    
    mu = sum(wts*mu)
    wts = as.matrix(wts)
    sd = as.numeric(sqrt((t(wts)%*%cov.mat%*%wts)))
    wts = as.numeric(wts)
    names(wts)= dimnames(returns)[[2]] }
   
   if(!is.null(digits)){
    out = list(WTS = wts,MU.PORT = mu,SD.PORT = sd)
    lapply(out,round,digits)}     else
    {if(wts.only) wts else mu}
}


minmu = function(returns, cset = NULL)
{
    gmv(returns,cset,wts.only = F)
}

# Max Mean Return Portfolio with Constraints
# This is primarily to compute the maximum mean return with constraints

maxmu = function(returns,cset,mu.only=TRUE,digits = NULL,verbose = FALSE)
{
  returns.old <- returns
  
#   if (any(c("turnover","propcost") %in% cset$clist.names)){
#     returns <- cbind(returns,returns,returns)      
#   } 
#   
  cov.mat <- cov(returns)
  Dmat <- 2*cov.mat
  #Make covariance positive definite
  #This should barely change the covariance matrix, as
  #the last few eigen values are very small negative numbers
  Dmat <- make.positive.definite(Dmat)
  mu <- apply(returns,2,mean)

  p = ncol(returns)
  if(is.null(cset))
  {A = matrix(rep(1,p),ncol =1)
   b = 1
   meq = 1}     else
   {
    A = cset$A
    b = cset$b
    meq = cset$meq
   }
    d = mu
    A = t(A)  # Because solve.QP uses the transpose of A
    if(meq>0)
    dir = c(rep("==",meq),rep(">=",length(b)-meq)) else
      dir = c(rep(">=",length(b)-meq)) 
#     
#     A <- A[-(1:4),]
#     b <- b[-(1:4)]
#     dir <- dir[-(1:4)]  
    port.maxmu = Rglpk_solve_LP(d,A,dir,b,max = TRUE,verbose = verbose)
  # Rglpk doesn't work with turnover/propcost
  if(port.maxmu$status!=0)
    warning("consider possibly relaxing constraints")
    wts = port.maxmu$solution      # Get optimal weights
    
#   
#   if (any(c("turnover","propcost") %in% cset$clist.names)){
#     wts = wts[1:(p/3)]+wts[(p/3+1):(p/3*2)]+wts[(p/3*2+1):p]
#     mu = sum(wts*mu[1:(p/3)])
#     wts = as.matrix(wts)
#     sd = as.numeric(sqrt((t(wts)%*%cov(returns.old)%*%wts)))
#     wts = as.numeric(wts)
#     names(wts)= dimnames(returns.old)[[2]]
#   } else{    
    mu = sum(wts*mu)
    wts = as.matrix(wts)
    sd = as.numeric(sqrt((t(wts)%*%cov.mat%*%wts)))
    wts = as.numeric(wts)
    names(wts)= dimnames(returns)[[2]] 

# }
  if(!is.null(digits)){
    out = list(WTS = wts,MU.PORT = mu,SD.PORT = sd)
    lapply(out,round,digits)}     else
    {if(mu.only) mu else wts}
}


# MVO Portfolio with Constraints
# If cset = NULL, then unconstrained mvo
# For back-test use default wts.only = T
# For efficient frontier use wts.only = F
# For  printout of wts, mu and sd use digits = 3 or 4

mvo = function(returns,mu0,cset=NULL,wts.only=T,digits = NULL)
{
 
  if(c("mu.target") %in% cset$clist.names)
    stop("mvo can not handel mu.target constraint")
  
  returns.old <- returns
  
  if (any(c("turnover","propcost") %in% cset$clist.names)){
    returns <- cbind(returns,returns,returns)      
  } 
  
  
  makenullmat <- function(A){matrix(0,nrow=nrow(A),ncol=ncol(A))}
  
  
  if ("turnover.doug" %in% cset$clist.names){
	  returns <- cbind(returns,makenullmat(returns),makenullmat(returns))      
  } 
  
  cov.mat <- cov(returns)
  Dmat <- 2*cov.mat
  #Make covariance positive definite
  #This should barely change the covariance matrix, as
  #the last few eigen values are very small negative numbers
  Dmat <- make.positive.definite(Dmat)
  mu <- apply(returns,2,mean)
 
  p = ncol(returns)
  if(is.null(cset))
  {A = cbind(mu,matrix(rep(1,p),ncol =1))
   b = c(mu0,1)
   meq = 2}     else
   {A = cbind(mu,cset$A)
    b = c(mu0,cset$b)
    meq = cset$meq+1}
  dvec <- rep(0,nrow(A)) #no linear part in this problem
  port.gmv = solve.QP(Dmat,dvec,A,b,meq)
  wts = port.gmv$solution      # Get optimal weights
  
  if (any(c("turnover","propcost") %in% cset$clist.names)){
    wts = wts[1:(p/3)]+wts[(p/3+1):(p/3*2)]+wts[(p/3*2+1):p]
    mu = sum(wts*mu[1:(p/3)])
    wts = as.matrix(wts)
    sd = as.numeric(sqrt((t(wts)%*%cov(returns.old)%*%wts)))
    wts = as.numeric(wts)
    names(wts)= dimnames(returns.old)[[2]]
  }      else if("turnover.doug" %in% cset$clist.names){
	  wts = wts[1:(p/3)]
	  mu = sum(wts*mu[1:(p/3)])
	  wts = as.matrix(wts)
	  sd = as.numeric(sqrt((t(wts)%*%cov(returns.old)%*%wts)))
	  wts = as.numeric(wts)
	  names(wts)= dimnames(returns.old)[[2]]
  }   else{    
    mu = sum(wts*mu)
    wts = as.matrix(wts)
    sd = as.numeric(sqrt((t(wts)%*%cov.mat%*%wts)))
    wts = as.numeric(wts)
    names(wts)= dimnames(returns)[[2]] }
  
  if(!is.null(digits)){
    out = list(WTS = wts,MU.PORT = mu,SD.PORT = sd)
    lapply(out,round,digits)}     else
    {if(wts.only) wts else c(mu,sd,wts)}
}
