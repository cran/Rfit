rfit.default <-
function (formula,  data=list(), yhat0 = NULL, 
    scores = wscores, symmetric = FALSE, intercept = NULL, TAU = 'F0',  ...) 

#  New in v0.16 (from v0.14):
#  1. added code to guess if an intercept should be fit:
#    if intercept == NULL then
#      if [1 x] is full rank then intercept is fit otherwise it is not
#    if intercept != NULL then do what the user asks.
#    intercept = FALSE returns regression through the origin estimate, 
#      but residuals, etc are not corret.
#  2. added options for two fortran versions of estimates of tau 

{


    mf <- model.frame(formula = formula, data = data)
	mt<-attr(mf,"terms")
	attr(mt,"intercept")<-0
    x <- model.matrix(mt, data = mf)
    y <- model.response(mf)

    x1 <- as.matrix(cbind(rep(1,nrow(x)),x))
    qrx <- qr(x1)
    Q<-as.matrix(qr.Q(qrx))
    q1<-Q[,1]
    xq<-as.matrix(Q[, 2:qrx$rank])

	if( is.null(intercept) ) {
		if(qrx$rank == ncol(x1)) {
			x<-x1
			intercept<-TRUE
		} else {
			intercept<-FALSE
		}
	} else {
	    if( intercept ) {
			 x<-x1
		} else { 
			warning("rfit: intercept = FALSE option in development stage")
		}
	}

    if( is.null(yhat0) ) {
        fit0<-suppressWarnings(rq(y~xq-1))
    } else {
        fit0 <- lsfit(xq, yhat0, intercept = FALSE)
    }
	ord<-order(fit0$resid)
    fit <- jaeckel(as.matrix(xq[ord,]), y[ord], fit0$coef, scores = scores)
    if( fit$convergence != 0 ) warning("rfit: Convergence status not zero in jaeckel")
	rm(ord)
    betahat <- fit$par

    yhat <- xq %*% betahat
    ehat <- y - yhat
    alphahat <- ifelse(symmetric, signedrank(ehat), median(ehat))
    ehat <- ehat - alphahat
    yhat<-yhat+alphahat

    bhat<-lsfit(x,yhat,intercept=FALSE)$coef

	r.gettau<-switch(TAU,
		F0 = gettauF0,
		R = gettau,
		N = function(...) NA
	)

    tauhat <- r.gettau(ehat, ncol(xq), scores)
    if (symmetric) {
        taushat <- tauhat
    } else {
        taushat <- taustar(ehat, qrx$rank)
    }

    res <- list( coefficients = bhat, residuals = ehat, fitted.values = yhat, 
        scores = scores, x = x, y = y, tauhat = tauhat, qrx1=qrx,
        taushat = taushat, symmetric = symmetric, betahat = bhat,disp=fit$value,intercept=intercept)
    res$call <- match.call()
    class(res) <- list("rfit")
    res
}
