irls <-
function (x, y, b0 = suppressWarnings(rq(y ~ x - 1)$coef), scores , 
    eps = 0.001, max.iter = 20) 
{
    x <- as.matrix(x)
    n <- length(y)
    p <- ncol(x)
    e0 <- drop(y - x %*% b0)
    w0 <- irlsweights(e0, scores)
    m <- median(e0)
    b1 <- lsfit(x, y - m, wt = w0, intercept = FALSE)$coef
    e1 <- drop(y - x %*% b1)
    m <- median(e1)
    iter <- 0
    while (sum((b0 - b1)^2)/sum(b1^2) > eps && iter < max.iter) {
        iter <- iter + 1
        b0 <- b1
        e0 <- e1
        b1 <- lsfit(x, y - m, wt = irlsweights(e0, scores), intercept = FALSE)$coef
        e1 <- drop(y - x %*% b1)
        m <- median(e1)
    }
    b1
}
