summary.rfit <-
function (object,...) 
{
#	fit<-object
    tauhat <- object$tauhat
    x <- as.matrix(object$x)
    n <- nrow(x)
    p <- ncol(x)
    est <- object$coef
    ses <- sqrt(diag(vcov(object)))
    tstat <- est/ses
    pval <- 2 * pt(-abs(tstat), n - p - 1)
    #betahat <- object$betahat
    coef <- cbind(est, ses, tstat, pval)
    colnames(coef) <- c("Estimate", "Std. Error", "t.value","p.value")
#    waldstat <- sum((x %*% betahat)^2)/(p * object$tauhat^2)
#    waldpval <- 1 - pf(waldstat, p, n - p - 1)
    dt <- drop.test(object)
    R2 <- (dt$df1/dt$df2 * dt$F)/(1 + dt$df1/dt$df2 * dt$F)
    ans <- list(coefficients = coef, dropstat = dt$F, droppval = dt$p.value, R2 = R2)
#    ans <- list(coefficients = coef, waldstat, waldpval = waldpval, dropstat = dt$F, droppval = dt$p.value, R2 = R2)
    ans$call <- object$call
    class(ans) <- "summary.rfit"
    ans
}
