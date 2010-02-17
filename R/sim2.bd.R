sim2.bd <-
function(n,age, lambda,mu){
	phy2 <- sim2.bd.origin(n,age,lambda,mu)
	if (class(phy2)=="phylo") {
		phy2<-collapse.singles(phy2)
	}
	phy2
	}

