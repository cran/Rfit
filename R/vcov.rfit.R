vcov.rfit <-
function (object, intercept = TRUE, ...)
#function (fit, intercept = TRUE) 
{
	fit<-object
    if (intercept) {
        xpxi <- chol2inv(chol(crossprod(fit$xc)))
        xbar <- apply(fit$x, 2, mean)
        kn <- fit$taushat^2/length(fit$resid) + fit$tauhat^2 * 
            t(xbar) %*% xpxi %*% xbar
        cvec <- -fit$tauhat^2 * t(xbar) %*% xpxi
        cbind(rbind(kn, t(cvec)), rbind(cvec, fit$tauhat^2 * 
            xpxi))
    }
    else {
        fit$tauhat^2 * chol2inv(chol(crossprod(fit$xc)))
    }
}
