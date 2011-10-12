# an example of match.arg 

confintadjust.methods<-c('none','bonferroni','tukey')

confintadjust<-function(n,k,alpha=0.05,method=confintadjust.methods,...) {
	method<-match.arg(method)
	cv<-switch(method, bonferroni = qt(1-alpha/choose(k,2),n-k),
		tukey = qtukey(1-alpha,k,n-k)/sqrt(2),
		none = qt(1-alpha/2,n-k)
	)

	res<-list(cv=cv,method=method)
	res

}


