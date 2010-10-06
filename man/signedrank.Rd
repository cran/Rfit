\name{signedrank}
\alias{signedrank}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Signed-Rank Estimate of Location (Intercept) }
\description{
Returns the signed-rank estimate of intercept with is equivalent to the Hodges-Lehmann estimate of the residuals.
}
\usage{
signedrank(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ numeric vector } 
}
%\details{
  %~~ If necessary, more details than the description above ~~
%}
\value{
  Returns the median of the Walsh averages.
}
\references{ 
Hettmansperger, T.P. and McKean J.W. (1998), \emph{Robust Nonparametric Statistical Methods}, London: Arnold.

Hollander, M. and Wolfe, D.A. (1999), \emph{Nonparametric Statistical Methods}, New York: Wiley.

}
\author{ John Kloke \email{jdk61@pitt.edu}}

%\note{ ~~further notes~~ 

 %~Make other sections like Warning with \section{Warning }{....} ~
%}
\seealso{ \code{\link{walsh}}}
\examples{

## The function is currently defined as
function (x) 
median(walsh(x))
}
