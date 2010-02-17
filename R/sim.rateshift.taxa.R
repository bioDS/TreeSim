sim.rateshift.taxa <-
function(n,numbsim,lambda,mu,frac,times,complete=TRUE){
	phy <- sim2.bd.rateshift(n,numbsim,lambda,mu,frac,times)
	if (complete == FALSE) {
		phy <- reconstructed.taxa(phy,(round(n/frac[1])-n))
		}
	phy
	}

