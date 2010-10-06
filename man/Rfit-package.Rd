\name{Rfit-package}
\alias{Rfit-package}
\alias{Rfit}
\docType{package}
\title{ Rank-Based Estimates and Inference for Linear Models}
\description{
Package provides functions for rank-based analyses of linear models.
Rank-based estimation and inference offers a robust alternative to least squares.
}
\details{
\tabular{ll}{
Package: \tab Rfit\cr
Type: \tab Package\cr
Version: \tab 0.08\cr
Date: \tab 2010-01-11\cr
License: \tab  GPL (version 2 or later)\cr
LazyLoad: \tab yes\cr
}
%~~ An overview of how to use the package, including the most important ~~
%~~ functions ~~
}
\author{
John Kloke, Joesph McKean

Maintainer: John Kloke <jdk61@pitt.edu>
}
\references{
Hettmansperger, T.P. and McKean J.W. (1998), \emph{Robust Nonparametric Statistical Methods}, London: Arnold.

Jaeckel, L. A. (1972). Estimating regression coefficients by minimizing the dispersion of residuals. \emph{Annal
s of Mathematical Statistics}, 43, 1449 - 1458.

Jureckova, J. (1971). Nonparametric estimate of regression coefficients. \emph{Annals of Mathematical Statistics
}, 42, 1328 - 1338.

}
%~~ Optionally other standard keywords, one per line, from file KEYWORDS in ~~
%~~ the R documentation directory ~~
\keyword{nonparametric}
\keyword{robust}
\keyword{regression}
\keyword{ package }
%\seealso{
%~~ Optional links to other man pages, e.g. ~~
%\code{\link[rfit]{rfit}}
%}
\examples{
data(baseball)
data(wscores)
fit<-rfit(weight~height,data=baseball)
summary(fit)
plot(fitted(fit),rstudent(fit))

### Example of the Reduction (Drop) in dispersion test ###
y<-rnorm(47)
x1<-rnorm(47)
x2<-rnorm(47)
fitF<-rfit(y~x1+x2)
fitR<-rfit(y~x1)
drop.test(fitF,fitR)









}