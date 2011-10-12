drop.test <-
function (fitF, fitR = NULL) {

    pp1 <- fitF$qrx1$rank

    if (is.null(fitR)) {
        rd <- disp(rep(0, ncol(fitF$x)), fitF$x, fitF$y, fitF$scores) - disp(fitF$betahat, fitF$x, fitF$y, fitF$scores)
        df1 <- pp1 - 1
    } else {
        rd <- disp(fitR$betahat, fitR$x, fitR$y, fitR$scores) - disp(fitF$betahat, fitF$x, fitF$y, fitF$scores)
        df1 <- length(fitF$betahat) - length(fitR$betahat)
    }

    df2 <- length(fitF$y) - pp1
    test <- (rd/df1)/(fitF$tauhat/2)
    pval <- 1 - pf(test, df1, df2)
    ans <- list(F = test, p.value = pval, RD = rd, tauhat = fitF$tauhat, df1 = df1, df2 = df2)
    class(ans) <- "drop.test"
    ans

}
