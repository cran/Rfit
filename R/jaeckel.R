jaeckel <-
function (x, y, beta0 = rq(y ~ x)$coef[2:(ncol(x) + 1)], scores = wscores) 
{
    optim(beta0, disp, method = "BFGS", x = x, y = y, scores = scores, 
        gr = grad)
}
