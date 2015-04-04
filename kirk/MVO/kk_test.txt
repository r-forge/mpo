# Turnover constraint comparion between doug's version and old version. 
# 
# Author: ke
###############################################################################


library(quadprog)
library(xts)
library(corpcor)
#source("mvo.constrained.r")
#source("efront.constrained.r")
#source("barplot.wts.r")

load("crsp.short.Rdata")
returns = midcap.ts[,1:5]
returns = coredata(returns)
#
mu.target = NULL
long.only = FALSE
toc=turnover=0.2
wts.initial=w.initial=rep(1/ncol(returns),ncol(returns))
res1 <- TurnoverOpt_doug(returns, mu.target =mu.target, wts.initial = wts.initial,toc = toc,long.only=long.only)
res2 <- TurnoverOpt(returns,mu.target=mu.target, w.initial=wts.initial,turnover=turnover,long.only=long.only)
RES <- rbind(c(res1$wts,res1$port.mu,res1$port.var,res1$turnover), c(res2$w,res2$port.mu, res2$port.var, res2$achieved.turnover))
colnames(RES) <- c(rep("wt",5),"mu","var","turnover")
RES
