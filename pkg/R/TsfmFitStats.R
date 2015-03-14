TsfmFitStats = function(x,rsq = F,digits = 3)
{
  fitSum = summary(x)
  out = data.frame(fitSum$sum.list[[1]]$coefficients)
  names(out) = c("Estimate","S.E.","t-Stat","p-Value")
  print(round(out,digits))
  if(rsq)
    {out = fitSum$sum.list[[1]]$r.squared
    out = data.frame(out,row.names = "R-Squared =")
    names(out) = ""
    round(out,digits)}
}
