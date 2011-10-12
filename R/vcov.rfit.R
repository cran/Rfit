vcov.rfit <-
function (object, intercept = NULL, ...)
#function (fit, intercept = TRUE) 
{

#    if( is.null(intercept) ) intercept<-object$intercept
#	fit<-object

	Q<-qr.Q(object$qrx1)
	q1<-Q[,1]
	Q2<-Q[,2:object$qrx1$rank]

	xxpxi<-object$x%*%chol2inv(chol(crossprod(object$x)))
	A1<-crossprod(q1,xxpxi) ; A2<-crossprod(Q2,xxpxi)
 	object$taushat^2*crossprod(A1)+object$tauhat^2*crossprod(A2)

}
