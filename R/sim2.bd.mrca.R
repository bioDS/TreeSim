sim2.bd.mrca <-
function(mrca,numbsim,lambda,mu){
	phy <- list()
	for (j in 1:numbsim) {
		
		stop = 0
		while (stop==0){
			t1 <- sim2.bd.origin(0,mrca,lambda,mu)
			if (class(t1) == "phylo"){
				stop =1
			} else if (t1 == 1) {
				stop = 1	
			}
		}
		stop = 0
		while (stop==0){
			t2 <- sim2.bd.origin(0,mrca,lambda,mu)
			if (class(t2) == "phylo"){
				stop =1
			} else if (t2 == 1) {
				stop = 1	
			}
		}		
		
		if (class(t1) == "phylo" && class(t2) == "phylo"){
			t<- bind.tree(t1,t2,where="root")
			t$tip.label <- paste("t", sample(t$Nnode+1), sep = "")
		} else if (class(t1) == "phylo" || class(t2) == "phylo")  {
			if (class(t2) == "phylo") {
				t1<-t2
				}
			t<-t1
			t$edge <- t1$edge + 1
			root<- t$edge[1,1]
			t$edge <- rbind(c(root,1),t$edge)
			t$edge.length <- c(mrca,t$edge.length)
			t$tip.label <- paste("t", sample(t$Nnode+1), sep = "")
		} else {
			t<- sim2.bd.fast(2,1,1,0,1)[[1]]
			t$edge.length <- t$edge.length*0+mrca
		} 
		phy <- c(phy,list(t))
	}
	phy	
	}

