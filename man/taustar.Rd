\name{taustar}
\alias{taustar}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Estimate of the Scale Parameter taustar }
\description{
An estimate of the scale parameter taustar =  1/(2*f(0)) is needed for the standard error of the intercept in rank-based regression.
}
\usage{
taustar(resid, p, conf = 0.95)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{resid}{  full model residuals }
  \item{p}{  is the number of regression coefficients (without the intercept) }
  \item{conf}{ confidence level of CI used }
}
\details{
Confidence interval estimate of taustar.
See, for example, Hettmansperger and McKean (1998) p.7-8 and p.25-26.
}
\value{
Length-one numeric object containing the estimated scale parameter taustar.
}

\references{ 
Hettmansperger, T.P. and McKean J.W. (1998), \emph{Robust Nonparametric Statisti
cal Methods}, London: Arnold.
}

\author{ Joseph McKean, John Kloke}
%\note{ ~~further notes~~ 

 %~Make other sections like Warning with \section{Warning }{....} ~
%}
\seealso{ \code{\link{rfit}} }
\examples{
##  This is an internal function.  See rfit for user-level examples.

## The function is currently defined as
function (resid, p, conf = 0.95) 
{
    n = length(resid)
    zc = qnorm((1 + conf)/2)
    c1 = (n/2) - ((sqrt(n) * zc)/2) - 0.5
    ic1 = floor(c1)
    if (ic1 < 0) {
        ic1 = 0
    }
    z = sort(resid)
    l = z[ic1 + 1]
    u = z[n - ic1]
    df = sqrt(n)/sqrt(n - p - 1)
    taustar = df * ((sqrt(n) * (u - l))/(2 * zc))
    taustar
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line