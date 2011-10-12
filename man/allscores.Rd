\name{allscores}
\alias{wscores}
\alias{nscores}
\alias{bentscores1}
\alias{bentscores2}
\alias{bentscores3}
\alias{bentscores4}
\docType{data}
\title{ All Scores }
\description{
An object of class scores which includes the score function and it's derivative for rank-based regression inference.
}
\usage{data(wscores)}
\format{
  The format is:
Formal class 'scores' [package ".GlobalEnv"] with 2 slots
  ..@ phi :function (u)  
  ..@ Dphi:function (u)  
}
\details{
 %~~ If necessary, more details than the __description__ above ~~
Using Wilcoxon (linear) scores leads to inference which has ARE of 0.955 to least squares (ML) when the data are normal.
Wilcoxon scores are optimal when the underlying error distribution is logistic.
Normal scores are optimal when the data are normally distributed.
}
%\source{
  %~~ reference to a publication or URL from which the data were obtained ~~
%}
\references{
Hettmansperger, T.P. and McKean J.W. (2011), \emph{Robust Nonparametric Statistical Methods, 2nd ed.}, New York: Chapman-Hall.
}
\examples{
data(wscores)
x<-runif(10)
y<-rlogis(10)
rfit(y~x,scores=wscores)
}
\keyword{datasets}
