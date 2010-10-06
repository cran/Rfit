summary.rfit <-
function (object,...) 
{
	fit<-object
    tauhat <- fit$tauhat
    x <- as.matrix(fit$xc)
    n <- nrow(x)
    p <- ncol(x)
    est <- fit$coef
    ses <- sqrt(diag(vcov(fit)))
    tstat <- est/ses
    pval <- 2 * pt(-abs(tstat), n - p - 1)
    betahat <- est[2:(p + 1)]
    coef <- cbind(est, ses, tstat, pval)
    colnames(coef) <- c("Estimate", "Std. Error", "t.value", 
        "p.value")
    waldstat <- sum((x %*% betahat)^2)/(p * fit$tauhat^2)
    waldpval <- 1 - pf(waldstat, p, n - p - 1)
    dt <- drop.test(fit)
    R2 <- (dt$df1/dt$df2 * dt$F)/(1 + dt$df1/dt$df2 * dt$F)
    ans <- list(coefficients = coef, waldstat = waldstat, waldpval = waldpval, 
        dropstat = dt$F, droppval = dt$p.value, R2 = R2)
    ans$call <- fit$call
    class(ans) <- "summary.rfit"
    ans
}
