sim.rateshift.taxa.help <-function(dummy,n,lambda,mu,frac,times,complete=TRUE){
	out<-sim.rateshift.taxa.loop(n,1,lambda,mu,frac,times,complete)
	out
	}