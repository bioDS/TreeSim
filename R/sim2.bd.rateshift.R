sim2.bd.rateshift <-
function(n,numbsim,lambda,mu,frac,times){	phy <- list()
	for (j in 1:numbsim){
		temp <- sim2.bd.rateshift.single(n,lambda,mu,frac,times)
		phy <- c(phy, list(temp))
		}
	phy
}

