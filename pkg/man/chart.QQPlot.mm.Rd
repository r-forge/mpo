\name{chart.QQPlot.mm}
\alias{chart.QQPlot.mm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
chart.QQPlot.mm
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
chart.QQPlot.mm(R, distribution = "norm", ylab = NULL, xlab = paste(distribution, "Quantiles"), main = NULL, las = par("las"), envelope = FALSE, labels = FALSE, col = c(1, 4), lwd = 2, pch = 1, cex = 1, line = c("quartiles", "robust", "none"), element.color = "darkgray", cex.axis = 0.8, cex.legend = 0.8, cex.lab = 1, cex.main = 1, xaxis = TRUE, yaxis = TRUE, ylim = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{R}{
%%     ~~Describe \code{R} here~~
}
  \item{distribution}{
%%     ~~Describe \code{distribution} here~~
}
  \item{ylab}{
%%     ~~Describe \code{ylab} here~~
}
  \item{xlab}{
%%     ~~Describe \code{xlab} here~~
}
  \item{main}{
%%     ~~Describe \code{main} here~~
}
  \item{las}{
%%     ~~Describe \code{las} here~~
}
  \item{envelope}{
%%     ~~Describe \code{envelope} here~~
}
  \item{labels}{
%%     ~~Describe \code{labels} here~~
}
  \item{col}{
%%     ~~Describe \code{col} here~~
}
  \item{lwd}{
%%     ~~Describe \code{lwd} here~~
}
  \item{pch}{
%%     ~~Describe \code{pch} here~~
}
  \item{cex}{
%%     ~~Describe \code{cex} here~~
}
  \item{line}{
%%     ~~Describe \code{line} here~~
}
  \item{element.color}{
%%     ~~Describe \code{element.color} here~~
}
  \item{cex.axis}{
%%     ~~Describe \code{cex.axis} here~~
}
  \item{cex.legend}{
%%     ~~Describe \code{cex.legend} here~~
}
  \item{cex.lab}{
%%     ~~Describe \code{cex.lab} here~~
}
  \item{cex.main}{
%%     ~~Describe \code{cex.main} here~~
}
  \item{xaxis}{
%%     ~~Describe \code{xaxis} here~~
}
  \item{yaxis}{
%%     ~~Describe \code{yaxis} here~~
}
  \item{ylim}{
%%     ~~Describe \code{ylim} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (R, distribution = "norm", ylab = NULL, xlab = paste(distribution, 
    "Quantiles"), main = NULL, las = par("las"), envelope = FALSE, 
    labels = FALSE, col = c(1, 4), lwd = 2, pch = 1, cex = 1, 
    line = c("quartiles", "robust", "none"), element.color = "darkgray", 
    cex.axis = 0.8, cex.legend = 0.8, cex.lab = 1, cex.main = 1, 
    xaxis = TRUE, yaxis = TRUE, ylim = NULL, ...) 
{
    x = checkData(R, method = "vector", na.rm = TRUE)
    if (is.null(main)) {
        if (!is.null(colnames(R)[1])) 
            main = colnames(R)[1]
        else main = "QQ Plot"
    }
    if (is.null(ylab)) 
        ylab = "Empirical Quantiles"
    result <- NULL
    line <- match.arg(line)
    good <- !is.na(x)
    ord <- order(x[good])
    ord.x <- x[good][ord]
    n <- length(ord.x)
    P <- ppoints(n)
    if (distribution == "mixnormal") {
        qmixnormal <- function(q, ...) {
            para = list(...)$para
            if (!is.list(para)) 
                stop(" 'para' must be a 'list' object")
            if (is.null(para$m) | is.na(para$m)) 
                stop("The number of component must be specified in 'para$m'")
            library(nor1mix)
            out = norMixEM(x, para$m, trace = 0)
            if (length(q) != 2) {
                print("fitted model:")
                print(out[1:para$m, ], digits = 3)
            }
            if (is.null(para$mu) | is.null(para$sig2)) {
                if (length(q) != 2) 
                  print("using fitted model as theoretical distribution")
                obj <- out
            }
            else {
                if (length(para$mu) != para$m | length(para$sig2) != 
                  para$m) 
                  stop("the number of components mismatch with parameter inputs")
                obj <- norMix(mu = para$mu, sig2 = para$sig2, 
                  w = para$w)
            }
            qnorMix(q, obj)
        }
        dmixnormal <- function(p, ...) {
            para = list(...)$para
            if (!is.list(para)) 
                stop(" 'para' must be a 'list' object")
            if (is.null(para$m) | is.na(para$m)) 
                stop("The number of component must be specified in 'para$m'")
            library(nor1mix)
            out = norMixEM(x, para$m, trace = 0)
            if (is.null(para$mu) | is.null(para$sig2)) {
                obj <- out
            }
            else {
                if (length(para$mu) != para$m | length(para$sig2) != 
                  para$m) 
                  stop("the number of components mismatch with parameter inputs")
                obj <- norMix(mu = para$mu, sig2 = para$sig2, 
                  w = para$w)
            }
            dnorMix(p, obj)
        }
    }
    q.function <- eval(parse(text = paste("q", distribution, 
        sep = "")))
    d.function <- eval(parse(text = paste("d", distribution, 
        sep = "")))
    z <- q.function(P, ...)
    plot(z, ord.x, xlab = xlab, ylab = ylab, main = main, las = las, 
        col = col[1], pch = pch, cex = cex, cex.main = cex.main, 
        cex.lab = cex.lab, axes = FALSE, ylim = ylim)
    if (line == "quartiles") {
        Q.x <- quantile(ord.x, c(0.25, 0.75))
        Q.z <- q.function(c(0.25, 0.75), ...)
        b <- (Q.x[2] - Q.x[1])/(Q.z[2] - Q.z[1])
        a <- Q.x[1] - b * Q.z[1]
        abline(a, b, col = col[2], lwd = lwd)
    }
    if (line == "robust") {
        stopifnot("package:MASS" \%in\% search() || require("MASS", 
            quietly = TRUE))
        coef <- coefficients(rlm(ord.x ~ z))
        a <- coef[1]
        b <- coef[2]
        abline(a, b, col = col[2])
    }
    if (line != "none" & envelope != FALSE) {
        zz <- qnorm(1 - (1 - envelope)/2)
        SE <- (b/d.function(z, ...)) * sqrt(P * (1 - P)/n)
        fit.value <- a + b * z
        upper <- fit.value + zz * SE
        lower <- fit.value - zz * SE
        lines(z, upper, lty = 2, lwd = lwd/2, col = col[2])
        lines(z, lower, lty = 2, lwd = lwd/2, col = col[2])
    }
    if (labels[1] == TRUE & length(labels) == 1) 
        labels <- seq(along = z)
    if (labels[1] != FALSE) {
        selected <- identify(z, ord.x, labels[good][ord])
        result <- seq(along = x)[good][ord][selected]
    }
    if (is.null(result)) 
        invisible(result)
    else sort(result)
    if (xaxis) 
        axis(1, cex.axis = cex.axis, col = element.color)
    if (yaxis) 
        axis(2, cex.axis = cex.axis, col = element.color)
    box(col = element.color)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
