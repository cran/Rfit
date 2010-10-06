\name{irls}
\alias{irls}
%\alias{irlsweights}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Function for Iterated Reweighted Least Squares R-type estimates}
\description{
This function is used to get close to the final solution.  From our empirical studies it is generally the case that the IRLS estimates get close to the final solution faster than other types of algorithms, however they do not seem to find the exact minimizer of Jaeckel's dispersion.  Hence a small number of these steps can be first used to speed up a Newton-type algorithm.

}
\usage{
irls(x, y, b0 = suppressWarnings(rq(y ~ x - 1)$coef), scores , eps = 0.001, max.iter = 20)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ n by p design matrix }
  \item{y}{ n by 1 response vector }
  \item{b0}{ initial estimate }
  \item{scores}{ object of class scores }
  \item{eps}{ stop if the relative change in the estimate is less than this value }
  \item{max.iter}{ maximum number of IRLS steps to take }
}
%\details{
  %~~ If necessary, more details than the description above ~~
%}
\value{
A vector of estimated regression coefficents.
}
\references{ 
Cheng, K. S. and Hettmansperger, T. P. (1983), Weighted Least-Squares Rank Regression, \emph{Communications in Statistics, Part A - Theory and Methods}, 12, 1069-1086.

Sievers, J. and Abebe, A. (2004), Rank Estimation of Regression Coefficients Using Iterated Reweighted Least Squares, \emph{Journal of Statistical Computation and Simulation}, 74, 821-831.
}

\author{ John Kloke \email{jdk61@pitt.edu}}

%\note{ ~~further notes~~ 

 %~Make other sections like Warning with \section{Warning }{....} ~
%}
\seealso{ \code{\link{rfit}}, \code{\link{jaeckel}}  }
\examples{
##  This is an internal function.  See rfit for user-level examples.

## The function is currently defined as
function (x, y, b0 = suppressWarnings(rq(y ~ x - 1)$coef), scores = wscores,
    eps = 0.001, max.iter = 20)
{
    x <- as.matrix(x)
    n <- length(y)
    p <- ncol(x)
    e0 <- drop(y - x \%*\% b0)
    w0 <- irlsweights(e0, scores)
    m <- median(e0)
    b1 <- lsfit(x, y - m, wt = w0, intercept = F)$coef
    e1 <- drop(y - x \%*\% b1)
    m <- median(e1)
    iter <- 0
    while (sum((b0 - b1)^2)/sum(b1^2) > eps && iter < max.iter) {
        iter <- iter + 1
        b0 <- b1
        e0 <- e1
        b1 <- lsfit(x, y - m, wt = irlsweights(e0, scores), intercept = F)$coef
        e1 <- drop(y - x \%*\% b1)
        m <- median(e1)
    }
    b1
  }


}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
