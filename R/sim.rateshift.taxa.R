sim.rateshift.taxa <-function(n,numbsim,lambda,mu,frac,times,complete=TRUE){
	out<-lapply(1:numbsim,sim.rateshift.taxa.help,n=n,lambda=lambda,mu=mu,frac=frac,times=times,complete=complete)
	out1<-lapply(out,function(x){ x[[1]][[1]]})
	out2<-sapply(out,function(x){ x[[2]]})
	out3<-list(out1,out2)
	out3
	}
