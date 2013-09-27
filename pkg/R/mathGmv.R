mathGmv <-
function(returns,digits = NULL)
{
	V <- var(returns)
	one <- rep(1, nrow(V))
	z <- solve(V, one)		# Compute z = V.inv * 1
	cc <- t(one) %*% z		# Compute cc = 1.transpose * V.inv * 1
	cc <- as.numeric(cc)	# Convert 1-by-1 matrix to a scalar
	wtsGmv <- z/cc
	mu <- apply(returns, 2, mean)
	a <- t(mu) %*% z
	muGmv <- as.numeric(a/cc)
	volGmv <- 1/cc^0.5
    if(is.null(digits))
        {out = list(wts = wtsGmv,mu = muGmv, vol = volGmv)}
        else
        {out = list(WTS.GMV = wtsGmv, MU.GMV = muGmv, VOL.GMV = volGmv)
         out = lapply(out,round,digits=digits)}
    out
}
