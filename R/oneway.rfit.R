oneway.rfit<-function(y,g,scores=wscores,p.adjust='none') {

	ug<-unique(g)
	K<-length(ug)
	nvec<-tapply(!is.na(y),g,sum)

	### R fit ###
	x<-model.matrix(~as.factor(g)-1)
	x<-x[,2:ncol(x)]
	fit<-rfit(y~x)
	deltahat<-fit$betahat[-1]
	tauhat<-fit$tauhat

	### R estimates of effect sizes ###
	kp<-length(deltahat)-1
	ind1<-rep(1:kp,times=seq(kp,1,by=-1))
	ind2<-ind1+sequence(seq(kp,1,by=-1))
	est<-c(deltahat,deltahat[ind1]-deltahat[ind2])

	### Standard Errors ###
	kp<-length(deltahat)
	ind1<-rep(1:kp,times=seq(kp,1,by=-1))
	ind2<-ind1+sequence(seq(kp,1,by=-1))
	se<-tauhat*sqrt(1/nvec[ind1]+1/nvec[ind2])

	### p-values ###
	pval<-p.adjust(pt(abs(est/se),length(y)-(kp+1)),method=p.adjust)
	pp<-matrix(nrow=K,ncol=K-1,dimnames=list(ug,ug[2:K]))
	pp[lower.tri(pp)]<-pval
    DNAME <- paste(deparse(substitute(y)), "and", deparse(substitute(g)))
	ans<-list(method="Rfit",data.name=DNAME,p.value=pp,p.adjust.method=p.adjust)
	class(ans)<-"pairwise.htest"

	res<-list(fit=fit,est=est,se=se,I=ug[ind1],J=ug[ind2],p.value=pval,pp=ans,y=y,g=g)
	res$call<-match.call()
	class(res)<-"oneway.rfit"
	res

}


