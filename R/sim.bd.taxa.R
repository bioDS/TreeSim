sim.bd.taxa <-
function(n,numbsim,lambda,mu,frac=1,complete=TRUE,stochsampling=FALSE){
	if (complete == TRUE) {
		phy <- sim2.bd.reverse(round(n/frac),numbsim,lambda,mu)
	} else if (stochsampling==FALSE) {
		phy <- sim2.bd.reverse(round(n/frac),numbsim,lambda,mu)
		phy <- reconstructed.taxa(phy,(round(n/frac)-n))
	} else {
		phy <- sim2.bd.fast(n,numbsim,lambda,mu,frac)
	}
	phy
	}

