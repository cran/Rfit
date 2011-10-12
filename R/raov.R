#raov<-function(f) {
raov<-function(f,data=list(),...) {

# input formula : y ~ a + b +...+ z
# where a,b,...,z are k factors
	
	mf <- model.frame(formula = f,data=data)
#	X<-mf[,seq(ncol(mf),2,by=-1)]
#	X<-mf[,seq(2,ncol(mf))]
#	mf<-mf[do.call(order,X),]
	nf<-apply(mf[,2:ncol(mf)],2,function(foo) length(unique(foo)))

#  nf is number number of levels in each of the factors
#  mf is a data.frame of the form [y,a,b,...,z] sorted by z,...,b,a

	fit<-kwayr(nf,mf)

	# set up column names
	fnames<-names(mf)
	fnames<-fnames[2:length(fnames)]

	ind<-subsets(length(fnames))

	foo<-function(x,a) { paste(a[x==TRUE],collapse=':') }
	rownames(fit$tab)<-apply(ind,1,foo,a=fnames)

	colnames(fit$tab)<-c('DF','RD','Mean RD','F', 'p-value')
	res<-list(table=fit$tab,residuals=residuals(fit$fit),fitted.values=fitted.values(fit$fit),fit=fit$fit)

	res$call<-match.call()
	class(res)<-"raov"

	res

}
