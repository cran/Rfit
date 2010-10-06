irlsweights <-
function (e, scores , eps = 1e-04) 
{
    r <- rank(e, ties.method = "first")/(length(e) + 1)
    t <- scores@phi(r)
    m <- median(e)
    w <- t/(e - m)
    w[(abs(e - m) < eps) == TRUE] <- 0
    w
}
