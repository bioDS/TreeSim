sim.bd.age.loop <-
function(age,numbsim,lambda,mu,frac=1,mrca=FALSE,complete=TRUE) {
	if (mrca == FALSE) {
		phy <- sim2.bd.age(age,numbsim,lambda,mu)
		if (complete==FALSE) {
			phy <- reconstructed.age(phy,frac)
		}
	} else {
		if (complete == TRUE) {
			phy <- sim2.bd.mrca(age,numbsim,lambda,mu)
		}	else {
			phy <- sim2.bd.mrca.reconst(age,numbsim,lambda,mu,frac)
			}
	}
	phy
	}

