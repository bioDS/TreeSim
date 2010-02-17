sim2.bd.rateshift.single <-
function(n,lambdavec,muvec,frac,times){
	check <- 0
	sumratiolam <- 0
	for (j in 1:length(lambdavec)){
		sumratiolam <- sumratiolam + 1/lambdavec[j]
		}
	while (check ==0){
	maxleaf <- round(n/frac[1])
	leaves<- 1:maxleaf
	nodes<- leaves
	timecreation <- 0*leaves
	edge <- vector()
	edge.length <- vector()
	newspecies<-0
	time<-0
	interval<-2
	extinct<-0
	doneME <-0
	lambda <- lambdavec[1]
	mu<-muvec[1]
	if (length(frac)==1){
		doneME<-1
		nextextinction <- 0
		} else {
		nextextinction=times[interval]	
			}
	while (length(leaves)>0){
	timestep <-rexp(1,(length(leaves)*(lambda+mu)))
	time <- time+timestep
	
	if (nextextinction>time || doneME==1){
	timecreation <- c(timecreation,time)
	specevent <- runif(1,0,1)   #event speciation or extinction
	if ((lambda/(lambda+mu)) >= specevent) { #speciation
		if (length(leaves)>1) {
		newspecies<- newspecies-1
		nodes<-c(nodes,newspecies)

		species <- sample((1:(length(leaves))),2) 
		spec1 <- (which(nodes==leaves[species[1]]))
		spec2 <- (which(nodes==leaves[species[2]]))
		
		leaves<- c(leaves,newspecies)
		edge<-rbind(c(newspecies,leaves[species[1]]),edge)
		edge.length<-c((time-timecreation[spec1]),edge.length)
		edge<-rbind(c(newspecies,leaves[species[2]]),edge)
		edge.length<-c((time-timecreation[spec2]),edge.length)
		if (species[1]>species[2]){
			leaves <- leaves[-species[1]]
			leaves <- leaves[-species[2]]
			} else {
			leaves <- leaves[-species[2]]
			leaves <- leaves[-species[1]]
				}
		} else {
		newspecies<- newspecies-1
		edge <- rbind(edge,c(newspecies,leaves[1]))
		spec <- (which(nodes==leaves[1]))
		edge.length<- c(edge.length,(time-timecreation[spec]))
		leaves <- leaves[-1]
		}
		} else {
			extinct <- extinct+1
			leaves<- c(leaves,(maxleaf+extinct))
			nodes<-c(nodes,(maxleaf+extinct))
			}
		} else {
		#mass extinction event
		time <- nextextinction
		nadd <- round(length(leaves)/frac[interval])
		if (nadd>length(leaves)){
		for (j in 1:(nadd-length(leaves))){
			timecreation <- c(timecreation,time)
			extinct <- extinct+1
			leaves<- c(leaves,(maxleaf+extinct))
			nodes<-c(nodes,(maxleaf+extinct))
			}
			}
		lambda<-lambdavec[interval]
		mu<-muvec[interval]
		if (interval == length(frac)){
			doneME<-1
			} else {
				interval<-interval+1
				nextextinction<-times[interval]
				}
		}
	}
	sampletree <- runif(1,0,1)
	if (sampletree < (1/lambda/(sumratiolam))){
		check <- 1
		}
	}


minval <- -min(edge)
for (j in 1:length(edge.length)){
	if (edge[j,1]<0){
		edge[j,1]<- edge[j,1]+minval+maxleaf+extinct+1
		}
	if (edge[j,2]<0){
		edge[j,2]<- edge[j,2]+minval+maxleaf+extinct+1
		}
	}

phy <- list(edge = edge)
phy$tip.label <- paste("t", sample(maxleaf+extinct), sep = "")

phy$edge.length <- edge.length
phy$Nnode <- maxleaf+extinct
class(phy) <- "phylo"
#phy2<-phy
phy2<-collapse.singles(phy)
phy2<-reorder(phy2)
phy2
}

