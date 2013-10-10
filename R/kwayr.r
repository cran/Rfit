#kwayr = function(levs,data,ci=F,ciconf=.95,cimat=c(0)){

# jk 06-19-2011: removed confidence interval 
kwayr = function(levs,data) {
#
#   Input:
#     levs = vector of levels corresponding to the factors A, B, C, etc.
#     data = matrix whose first column is the response variable then the succeeding columns
#            are the level of the first factor, second factor, etc.
#            For example, the Data 
#              for a three way is of the form:
#              Y_ijkl i j k 
#            NOTE: The data must be presorted so that k runs the fastest, than j, than i
#     CONTRASTS = T if confidence intervals are to be obtained.
#     CONTRASTSCONF is the confidence coefficient for the confidence intervals for
#                   the contrasts.
#     CONTRASTSMAT is the matrix of contrasts, one row for each contrast.
#

    nf = length(levs)
    y = data[,1]
    levsind = data[,2:(nf+1)]
    n = length(y)
    xfull = cellx(data[,2:(nf+1)])
    listtests = subsets(nf)
    p = length(xfull[1,])
     xuse = xfull
     fitF = rfit(y~xfull-1)
     iflagq = 0

    drf = disp(fitF$betahat, xuse, fitF$y, fitF$scores)
    ehatr = fitF$resid
    dfull = n - p
    nt = length(listtests[,1])
    tab2 = matrix(rep(0,nt*5),ncol=5)
    tab3 = matrix(rep(0,nt*5),ncol=5)

    for(i in 1:nt){
      permh = listtests[i,]
      hmat = khmat(levsind,permh)
      q = length(hmat[,1])
#      see = cbind(rep(i,q),hmat)
#      write(t(see),ncol=13,append=T,file="allh.dat")
      xred = redmod(xfull,hmat)
      fitr = rfit(y~xred-1)
      drr = disp(fitr$betahat, xred, fitr$y, fitr$scores)
      rd = drr - drf
      ft = (rd/q)/(fitF$tauhat/2)
      pv = 1 - pf(ft,q,dfull)
      tab2[i,] = c(q,rd,(rd/q),ft,pv)
#      tab3[i,] = c(dfull,0,(fitF$tauhat/2),drr,drf)
    }
	list(tab=tab2,fit=fitF)
#         list(listtests=listtests,tab2=tab2,tab3=tab3,ci=ci)
 
}
