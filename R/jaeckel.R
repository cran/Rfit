jaeckel <- function (x, y, beta0 = rq(y ~ x)$coef[2:(ncol(x) + 1)], scores = wscores) {

	scrs<-getScores(scores,seq_len(length(y))/(length(y)+1))

	j.grad <- function (x, y, beta,scores,...) {
    	x <- as.matrix(x)
    	e <- y - x %*% beta
    	r <- rank(e, ties.method = "first")/(length(e) + 1)
#    	-t(x) %*% getScores(scores,r)
    	crossprod(x,-1*getScores(scores,r) )
	}


	j.disp <- function (beta, x, y,scrs,...) {
    	e <- y - x %*% beta
		drop(crossprod(e[order(e)],scrs))
	}

	optim(beta0, j.disp, method = "BFGS", x = x, y = y, scrs=scrs,gr=j.grad,scores=scores)

}

