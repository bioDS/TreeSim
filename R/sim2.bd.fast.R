sim2.bd.fast <-
function(n,numbsim,lambda,mu,rho){	phy <- list()
	for (j in 1:numbsim){
		temp <- sim2.bd.fast.single(n,lambda,mu,rho)
		phy <- c(phy, list(temp))
		}
	phy
}

