drop.test <-
function (fitF, fitR = NULL) 
{
    if (is.null(fitR)) {
        x <- fitF$xc
        p <- ncol(x)
        rd <- disp(rep(0, p), x, fitF$y, fitF$scores) - disp(fitF$betahat, 
            x, fitF$y, fitF$scores)
        df1 <- p
        df2 <- length(fitF$y) - p - 1
    }
    else {
        rd <- disp(fitR$betahat, fitR$xc, fitR$y, fitR$scores) - 
            disp(fitF$betahat, fitF$xc, fitF$y, fitF$scores)
        df1 <- length(fitF$betahat) - length(fitR$betahat)
        df2 <- length(fitF$y) - ncol(fitF$x) - 1
    }
    test <- (rd/df1)/(fitF$tauhat/2)
    pval <- 1 - pf(test, df1, df2)
    ans <- list(F = test, p.value = pval, RD = rd, tauhat = fitF$tauhat, 
        df1 = df1, df2 = df2)
    class(ans) <- "drop.test"
    ans
}
