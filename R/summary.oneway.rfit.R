summary.oneway.rfit<-function(object,alpha=0.05,method=confintadjust.methods,...) {

	method<-match.arg(method)
	cv<-confintadjust(length(object$y),length(unique(object$g)),alpha,method)
	x<-cbind.data.frame(object$I,object$J,object$est,object$se,object$est-cv$cv*object$se,object$est+cv$cv*object$se)
	names(x)<-c('I','J','Estimate','St Err','Lower Bound CI', 'Upper Bound CI')
	res<-list(table=x,method=method)
	class(res)<-'summary.oneway.rfit'
	res

}
