\name{mathGmv}
\alias{mathGmv}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
mathGmv
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
mathGmv(returns, digits = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{returns}{
%%     ~~Describe \code{returns} here~~
}
  \item{digits}{
%%     ~~Describe \code{digits} here~~
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
function (returns, digits = NULL) 
{
    V <- var(returns)
    one <- rep(1, nrow(V))
    z <- solve(V, one)
    cc <- t(one) \%*\% z
    cc <- as.numeric(cc)
    wtsGmv <- z/cc
    mu <- apply(returns, 2, mean)
    a <- t(mu) \%*\% z
    muGmv <- as.numeric(a/cc)
    volGmv <- 1/cc^0.5
    if (is.null(digits)) {
        out = list(wts = wtsGmv, mu = muGmv, vol = volGmv)
    }
    else {
        out = list(WTS.GMV = wtsGmv, MU.GMV = muGmv, VOL.GMV = volGmv)
        out = lapply(out, round, digits = digits)
    }
    out
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
