\name{taufuncs}
\alias{hstarready2}
\alias{hstar}
\alias{looptau}
\alias{pairup}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Internal Functions for Estimating tau }
\description{
These are internal functions used for calculating the scale parameter tau necessary for estimating the standard errors of coefficients for rank-regression.
}
\usage{
hstarready2(ehat,phi,phipr,param=c(0))
hstar(abdord, wtord, const, n, y) 
looptau(delta, abdord, wtord, const, n, param = c(0))
pairup(x,type="less") 
}
\references{ 
Hettmansperger, T.P. and McKean, J.W. (1998), \emph{Robust Nonparametric Statistical Methods}, London: Arnold.

Koul, H.L., Sievers, G.L., and McKean, J.W. (1987) An esimator of the scale parameter for the rank analysis of linear models under general score functions, \emph{Scandinavian Journal of Statistics}, 14, 131-141.
}
\author{ Joseph McKean, John Kloke}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ehat}{ Full model residals }
  \item{phi}{ Score function used }
  \item{phipr}{ Phi Prime }
  \item{param}{ Joe's param }
  \item{delta}{ Ask Joe }
  \item{y}{ Ask Joe }
  \item{abdord}{ Ask Joe }
  \item{wtord}{ Ask Joe }
  \item{const}{ Ask Joe }
  \item{n}{ Ask Joe }
  \item{x}{ Ask Joe }
  \item{type}{ Ask Joe }
}
%\details{
  %~~ If necessary, more details than the description above ~~
%}
%\value{
  %~Describe the value returned
  %If it is a LIST, use
  %\item{comp1 }{Description of 'comp1'}
  %\item{comp2 }{Description of 'comp2'}
  %...
%}

\seealso{ \code{\link{gettau}}, \code{\link{rfit}} }
