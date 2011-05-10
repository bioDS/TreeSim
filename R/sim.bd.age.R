sim.bd.age <-function(age,numbsim,lambda,mu,frac=1,mrca=FALSE,complete=TRUE){
	out<-lapply(1:numbsim,sim.bd.age.help,age=age,lambda=lambda,mu=mu,frac=frac,mrca=mrca,complete=complete)
	out
	}