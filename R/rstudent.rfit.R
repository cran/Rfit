rstudent.rfit <-
function (model,...) 
{
	fit<-model
    ehat <- fit$resid
    n <- length(ehat)
    p <- fit$qrx1$rank-1
    sigmahat <- mad(ehat)
    deltas <- sum(abs(ehat))/(n - p)
    delta <- disp(fit$betahat, fit$x, fit$y, fit$scores)/(n - p)
    k2 <- (fit$tauhat/sigmahat)^2 * (2 * delta/fit$tauhat - 1)
    if (fit$symmetric) {
        h <- hat(fit$x)
        s <- suppressWarnings(sigmahat * sqrt(1 - k2 * h))
        s[is.na(s)] <- sigmahat * sqrt(1 - h)[is.na(s)]
    }
    else {
        hc <- hat(as.matrix(qr.Q(fit$qrx1)[,2:(p+1)]), intercept=FALSE)
        k1 <- (fit$taushat/sigmahat)^2 * (2 * deltas/fit$taushat - 1)
        s <- suppressWarnings(sigmahat * sqrt(1 - k1/n - k2 * hc))
        s[is.na(s)] <- sigmahat * sqrt(1 - hc)[is.na(s)]
    }
    ehat/s
}
