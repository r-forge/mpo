sfmFitStats = function(x,rsq = F,digits = 3)
{
  outSum = summary(x)
  out = data.frame(outSum$coefficients[,1:3])
  names(out) = c("Estimate","StdError","t-Value")
  row.names(out) = c("Alpha","Beta")
  print(round(out,digits))
  if(rsq)
    {out = outSum$r.squared
    out = data.frame(out,row.names = "R-Squared =")
    names(out) = ""
    round(out,digits)}
}
