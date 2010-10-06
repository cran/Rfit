\name{gettau}
\alias{gettau}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Estimate of the Scale Parameter tau }
\description{
An estimate of the scale parameter tau is needed for the standard errors of the coefficents in rank-based regression.
}
\usage{
gettau(ehat, p, phi , phidu, param = c(0), delta = 0.8, hparm = 2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ehat}{ full model residuals }
  \item{p}{ number of regression coefficents }
  \item{phi}{ score function used in estimation }
  \item{phidu}{ derivative of the score function \code{phi} }
  \item{param}{ Joe's params }
  \item{delta}{ confidence level }
  \item{hparm}{ Joe's hparm }
}
\details{
%  ~~ If necessary, more details than the description above ~~
This is the confidence interval type estimate of the scale parameter tau developed my Koul, Sievers, and McKean (1987).
This estimate is also discussed in Section 3.7.1 of Hettmansperger and McKean (1998).
}
\value{
Length one numeric object.
}
\references{ 
Hettmansperger, T.P. and McKean, J.W. (1998), \emph{Robust Nonparametric Statistical Methods}, London: Arnold.

Koul, H.L., Sievers, G.L., and McKean, J.W. (1987) An esimator of the scale parameter for the rank analysis of linear models under general score functions, \emph{Scandinavian Journal of Statistics}, 14, 131-141.
}
\author{ Joseph McKean, John Kloke}
%\note{ ~~further notes~~ 

 %~Make other sections like Warning with \section{Warning }{....} ~
%}
\seealso{ \code{\link{rfit}} }
%\examples{
%
%}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
