tsPlot = function(ret,add.grid = T)
{
  if(add.grid) {type = c("l","g")} else
  {type = "l"}
  xyplot(100*retVHI[,1:2],par.strip.text = list(cex = 1.5),type = type,
         xlab = "", ylab = list(label="RETURNS (%)",cex=1.3),
         scales = list(y = list(cex = 1.5),x = list(cex = 1.5) )) 
}
