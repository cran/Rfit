subsets<-function(k){
     subsets = expand.grid(rep(list(0:1),k))
     ad  = apply(subsets,1,sum)
     see = order(ad)
     subsets = subsets[see,]
     n = length(see)
     subsets = subsets[2:n,]
     subsets
}
