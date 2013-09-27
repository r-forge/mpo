opt.outputMeanVolWts <-
function(opt,returns,digits = NULL,names = NULL)
{
wts = opt$weights
sigmasq = as.numeric(t(wts)%*%var(returns)%*%wts)
sigma = sqrt(sigmasq)
mu.ret = apply(returns,2,mean)
mu = as.numeric(t(wts)%*%mu.ret)
if(is.null(digits))
    {output = wts} else
    {if(is.null(names))
        {output = list(wts=wts)
         output = lapply(output,round,digits)}
         else
        {output = list(wts=wts)
         names(output) = names
         output = lapply(output,round,digits)}
    }
output
}
