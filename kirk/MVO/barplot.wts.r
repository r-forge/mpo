barplot.wts = function(wts.efront,legend.text = NULL,col = NULL,ylab = NULL ,xlab = c("MU","VOL"),bar.ylim = NULL)
{
  xlab.choose <- match.arg(xlab)
#   cat(xlab.choose,"\n")
  xlab <- wts.efront[xlab.choose,]
  xlab <- round(xlab,4)
  xlab <- sprintf("%.4f", xlab)
  x <- wts.efront[-c(1,2),]
  n = ncol(x); p = nrow(x)
  xpos = (abs(x)+x)/2
  xneg = (x-abs(x))/2
  if(is.null(bar.ylim))
  {ymax <- max(colSums(xpos,na.rm=T))
   ymin <- min(colSums(xneg,na.rm=T))
   ylim = c(ymin,ymax)}   else {ylim = bar.ylim}
  colnames(xpos) <- xlab
  barplot(xpos,legend.text = legend.text,col = col,ylab = ylab,xlab = xlab.choose,
		  ylim = bar.ylim, las=2, cex.names=0.8, bty="n")
  barplot(xneg,add = T,col = col,axisnames=FALSE,axes=FALSE)
  abline(h=0)
}
# 
# x = cbind(c(.1,.3,.8,-.2),c(.2,.3,.2,.3))
# barplot.wts(x,legend.text = T, col = topo.colors(4),ylab = "WEIGHTS", xlab = "VOL",
#              bar.ylim = c(0,2))
