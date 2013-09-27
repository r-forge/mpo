barplot.wts <-
function(x,legend.text = NULL,col = NULL,ylab = NULL ,xlab = NULL,bar.ylim = NULL)
{
    n = ncol(x); p = nrow(x)
    xpos = (abs(x)+x)/2
    xneg = (x-abs(x))/2
    if(is.null(bar.ylim))
    {ymax <- max(colSums(xpos,na.rm=T))
    ymin <- min(colSums(xneg,na.rm=T))
    ylim = c(ymin,ymax)}
    else {ylim = bar.ylim}
    barplot(xpos,legend.text = legend.text,col = col,ylab = ylab,xlab = xlab,
             args.legend = list(x=1.2,y=bar.ylim[2],cex = .5), ylim = bar.ylim)
    barplot(xneg,add = T,col = col)
    abline(h=0)
}
