rfit.default <-
function (formula,  data=list(), yhat0 = NULL, 
    scores = wscores, symmetric = FALSE, intercept = TRUE, ...) 

# TODO: Add intercept = NULL then try to guess if the intercept 
#   should be fit based on whether a one is in the col space.
#   If intercept != NULL then do what the user asks.

# use intercept = FALSE when 1 is in the column space of X 
# or for regression through the origin
{



    mf <- model.frame(formula = formula, data = data)
	mt<-attr(mf,"terms")
	attr(mt,"intercept")<-0
    x <- model.matrix(mt, data = mf)
#    x <- x[, 2:ncol(x)]
    y <- model.response(mf)

    x1 <- as.matrix(cbind(rep(1,nrow(x)),x))
    if( intercept ) x<-x1

#    x1 <- as.matrix(ifelse( intercept,cbind(rep(1,nrow(x)),x), x)
#    xbar <- apply(x, 2, mean)
#    xc <- as.matrix(x - outer(rep(1, nrow(x)), xbar))
    qrx <- qr(x1)
    Q<-as.matrix(qr.Q(qrx))
    q1<-Q[,1]
    xq<-as.matrix(Q[, 2:qrx$rank])

    if( is.null(yhat0) ) {
        beta0<-suppressWarnings(rq(y~xq-1)$coef)
    } else {
        beta0 <- lsfit(xq, yhat0, intercept = FALSE)$coef
    }

    fit <- jaeckel(xq, y, beta0, scores = scores)
    betahat <- fit$par
    yhat <- xq %*% betahat
    ehat <- y - yhat
    alphahat <- ifelse(symmetric, signedrank(ehat), median(ehat))
    ehat <- ehat - alphahat
    yhat<-yhat+alphahat

#	thetahat<-t(Q)%*%yhat
    bhat<-lsfit(x,yhat,intercept=FALSE)$coef

#    betahat <- lsfit(x, yhat, intercept = FALSE)$coef
    tauhat <- gettau(ehat, ncol(xq), scores)
    if (symmetric) {
        taushat <- tauhat
    }
    else {
        taushat <- taustar(ehat, qrx$rank)
    }
#    if( intercept ) {
#       coef <- c(alphahat, bhat)
#    } else {
       coef<-bhat
#    }

    res <- list( coefficients = coef, residuals = ehat, fitted.values = yhat, 
        scores = scores, x = x, y = y, tauhat = tauhat, qrx1=qrx,
        taushat = taushat, symmetric = symmetric, betahat = bhat,disp=fit$value )
    res$call <- match.call()
    class(res) <- list("rfit")
    res
}
