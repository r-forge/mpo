barplot.wts = function(x,legend.text = NULL,col = NULL,ylab = NULL ,xlab = NULL,bar.ylim = NULL)
{
  n = ncol(x); p = nrow(x)
  xpos = (abs(x)+x)/2
  xneg = (x-abs(x))/2
  if(is.null(bar.ylim))
  {ymax <- max(colSums(xpos,na.rm=T))
   ymin <- min(colSums(xneg,na.rm=T))
   ylim = c(ymin,ymax)}   else {ylim = bar.ylim}
  barplot(xpos,legend.text = legend.text,col = col,ylab = ylab,xlab = xlab,
          ylim = bar.ylim, las=2)
  axis(1,labels=colnames(xpos),las=2)
  barplot(xneg,add = T,col = col,axisnames=FALSE)
  abline(h=0)
}
# 
# x = cbind(c(.1,.3,.8,-.2),c(.2,.3,.2,.3))
# barplot.wts(x,legend.text = T, col = topo.colors(4),ylab = "WEIGHTS", xlab = "VOL",
#              bar.ylim = c(0,2))
