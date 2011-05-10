sim.bd.taxa <-function(n,numbsim,lambda,mu,frac=1,complete=TRUE,stochsampling=FALSE){
	out<-lapply(1:numbsim,sim.bd.taxa.help,n=n,lambda=lambda,mu=mu,frac=frac,complete=complete,stochsampling=stochsampling)
	out1<-lapply(out,function(x){ x[[1]][[1]]})
	out2<-sapply(out,function(x){ x[[2]]})
	out3<-list(out1,out2)
	out3
	}
