rfit.default <-
function (formula,  data=list(), yhat0 = suppressWarnings(rq(y ~ x)$fitted.values), 
    scores = wscores, symmetric = FALSE, ...) 
{

    mf <- model.frame(formula = formula, data = data)
    x <- model.matrix(attr(mf, "terms"), data = mf)
    x <- x[, 2:ncol(x)]
    y <- model.response(mf)

    x <- as.matrix(x)
    xbar <- apply(x, 2, mean)
    xc <- as.matrix(x - outer(rep(1, nrow(x)), xbar))
    qrx <- qr(xc)
    xq <- as.matrix(qr.Q(qrx)[, 1:qrx$rank])
    beta0 <- lsfit(xq, yhat0, intercept = FALSE)$coef
    fit <- jaeckel(xq, y, beta0, scores = scores)
    betahat <- fit$par
    yhat <- xq %*% betahat
    betahat <- lsfit(xc, yhat, intercept = FALSE)$coef
    ehat <- y - yhat
    alphahat <- ifelse(symmetric, signedrank(ehat), median(ehat))
    ehat <- ehat - alphahat
    tauhat <- gettau(ehat, ncol(x), scores@phi, scores@Dphi)
    if (symmetric) {
        taushat <- tauhat
    }
    else {
        taushat <- taustar(ehat, ncol(x))
    }
    alphahat <- alphahat - sum(xbar * betahat)
    coef <- c(alphahat, betahat)
    res <- list(coefficients = coef, residuals = ehat, fitted.values = y - 
        ehat, scores = scores, x = x, y = y, xc = xc, tauhat = tauhat, 
        taushat = taushat, symmetric = symmetric, betahat = betahat)
    res$call <- match.call()
    class(res) <- list("rfit")
    res
}
