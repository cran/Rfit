\name{irlsweights}
\alias{irlsweights}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{  Function for Iterated Reweighted Least Squares R-type estimates  }
\description{
Function returns the rank-based weights for the IRLS algorithm.
}
\usage{
irlsweights(e, scores, eps = 1e-04)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{e}{ residuals }
  \item{scores}{ object of class scores }
  \item{eps}{ residuals less than this have weights set to 0 }
}
%\details{
  %~~ If necessary, more details than the description above ~~
%}
\value{
A vector of weights to used in \code{irls}
}
%\references{ ~put references to the literature/web site here ~ }
\references{
Cheng, K. S. and Hettmansperger, T. P. (1983), Weighted Least-Squares Rank Regression, \emph{Communications in Statistics, Part A - Theory and Methods}, 12, 1069-1086.

Sievers, J. and Abebe, A. (2004), Rank Estimation of Regression Coefficients Using Iterated Reweighted Least Squares, \emph{Journal of Statistical Computation and Simulation}, 74, 821-831.
}

%\author{ ~~who you are~~ }
\author{ John Kloke \email{jdk61@pitt.edu}}

%\note{ ~~further notes~~ 

 %~Make other sections like Warning with \section{Warning }{....} ~
%}
\seealso{ \code{\link{rfit}}, \code{\link{jaeckel}}  }

%\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
##  This is a internal function.  See rfit for user-level examples.

## The function is currently defined as
function (e, scores = wscores, eps = 1e-04) 
{
    r <- rank(e, ties.method = "first")/(length(e) + 1)
    t <- scores@phi(r)
    m <- median(e)
    w <- t/(e - m)
    w[(abs(e - m) < eps) == T] <- 0
    w
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
