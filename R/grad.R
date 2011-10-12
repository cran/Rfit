grad <-
function (x, y, beta, scores) 
{
    x <- as.matrix(x)
    e <- y - x %*% beta
    r <- rank(e, ties.method = "first")/(length(e) + 1)
#    -t(x) %*% scores@phi(r)
	-t(x) %*% getScores(scores,r)
}
