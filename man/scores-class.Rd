\name{scores-class}
\Rdversion{1.1}
\docType{class}
\alias{scores-class}

\title{Class "scores"}
\description{
A score function and it's corresponding derivative is required for rank-based estimation.  
This object puts them together.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("scores", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{phi}:}{Object of class \code{"function"} the score function }
    \item{\code{Dphi}:}{Object of class \code{"function"} the first derivative of the score function }
  }
}
\section{Methods}{
No methods defined with class "scores" in the signature.
}
\references{
Hettmansperger, T.P. and McKean J.W. (1998), \emph{Robust Nonparametric Statistical Methods}, London: Arnold.
}
\author{ John Kloke \email{jdk61@pitt.edu}}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%%  ~~objects to See Also as \code{\link{~~fun~~}}, ~~~
%%  ~~or \code{\linkS4class{CLASSNAME}} for links to other classes ~~~
%}
\examples{
showClass("scores")
}
\keyword{classes}
