sim.bdsky.stt <- function(n,lambdasky,deathsky,timesky,sampprobsky,rho=0,timestop=0){
	detail<-0
	psampsky<-sampprobsky
	musky<-deathsky*(1-psampsky)
	psisky<-deathsky*(psampsky)
	extant<-0
	if (rho>0 && timestop==0){print("rho > 0 and timestop = 0 is not a valid stopping condition")
		return()
		}
	if (n==0 && timestop==0){print("n = 0 and timestop = 0 is not a valid stopping condition")
		return()
		}
	if (n>0 && timestop>0){print("n > 0 and timestop > 0 are two stopping condition. Condition n is ignored.")
		}

			
	timesky<-c(timesky,10^100)
	timesky<-timesky[-1]
	extincttree = 1

	while(extincttree==1){
	edge <- c(-1,-2)		#matrix of edges
	leaves <- c(-2)			#list of extant leaves
	sampled <- vector()
	timecreation <-c(0,0)	#time when species -2, -3, ... -n was created after origin
	extinct <- vector()		#list of extinct leaves
	time <-0				#time after origin
	maxspecies <- -2		#smallest species
	edge.length <- c(0)		#edge length. if 0: leaf which didn't speciate /extinct yet
	extincttree = 0
	stop = 0
	
	lambda<-lambdasky[1]
	mu<-musky[1]
	psi<-psisky[1]
	timecut<-timesky[1]
	shift<-2
	while (stop == 0 ){
		if (length(leaves) == 0){
			phy2=0
			extincttree=1
			print("extinct tree")
			if (timestop>0) {phy2<-0
				return(phy2)
				}
			stop = 1
		} else {	
			timestep <- rexp(1,(length(leaves)*(lambda+mu+psi)))    #time since last event
			time = time+timestep			#time after origin
			if (timestop>0 && time>timestop){
				stop =1
			}else{			
			if (time<timecut){
			species <- sample(leaves,1)  	#the leaf undergoing the next event
			del <- which(leaves == species)
			specevent <- runif(1,0,1)		#event speciation, sampling or extinction
			edgespecevent <- which(edge == species) - length(edge.length)
			if ((lambda/(lambda+mu+psi)) > specevent) {
				edge.length[edgespecevent] <- time-timecreation[- species]
				edge <- rbind(edge,c(species,maxspecies-1))
				edge <- rbind(edge,c(species,maxspecies-2))
				edge.length <- c(edge.length,0,0)
				leaves <- c(leaves,maxspecies-1,maxspecies-2)
				maxspecies <- maxspecies-2
				leaves <- leaves[- del]
				timecreation <- c(timecreation,time,time)}
			else if (((lambda+psi)/(lambda+mu+psi)) > specevent) {	
				sampled<-c(sampled,leaves[del])
				leaves <- leaves[- del]
				edge.length[edgespecevent] <- time-timecreation[- species]
				if (length(sampled) == n && timestop==0){
					stop = 1
				}
			} else {
				extinct <- c(extinct,leaves[del])
				leaves <- leaves[- del]
				edge.length[edgespecevent] <- time-timecreation[- species]
			}
			}else{
				time<-timecut
				timecut<-timesky[shift]
				lambda<-lambdasky[shift]
				mu<-musky[shift]
				psi<-psisky[shift]
				shift<-shift+1
			}		
		}}
	}}
	if (rho==0){
	extant<-(length(leaves))
	while (length(leaves)>0) {
		del<-1
		extinct <- c(extinct,leaves[del])
		k = which( edge == leaves[del]  ) - length(edge.length)
		edge.length[k] <- time-timecreation[- leaves[del]]
		leaves <- leaves[- del]
	}
	} else {
		#print(leaves)
		time<-timestop
		todiesample = runif(length(leaves),0,1)
		todie<- which(todiesample>rho)
		#print(extinct)
		extinct<-c(extinct,leaves[todie])
		#print(extinct)
		if (length(todie)==length(leaves)){print("no sampled extant individuals")}
		for (j in (1:length(leaves))){
			k = which( edge == leaves[j]  ) - length(edge.length)
			edge.length[ k ] <- time - timecreation[- leaves[j]]
		}
		if (length(todie)>0){
			leaves<-leaves[-todie]}
		#print(todie)
		#print(leaves)	
		sampled<-c(sampled,leaves)
		leaves<-vector()
	}
	
	if (length(sampled)>0) {
	if(length(extinct>0)){
	for (j in 1:length(extinct)){
		del<-which (edge==extinct[j]) - length(edge.length)
		surpress<-edge[del,1]
		edge.length<-edge.length[- del]
		edge<-edge[- del,]
		del2<-which (edge[,1]==surpress)
		modify <-which (edge[,2]==surpress)
		edge[modify,2]<-edge[del2,2]
		edge.length[modify]<-edge.length[modify]+edge.length[del2]
		edge.length<-edge.length[- del2]
		edge<-edge[- del2,]
	}}

	nodes <- (length(sampled))*2
	leaf=1
	interior=length(sampled)+1
	edgetemp<-edge
		
	if (length(sampled)==1){
		print("only one sampled individual")
		return(edge.length)} 
		
	temp<-unique(c(edge))
	temp<-sort(temp,decreasing=TRUE)
	for (j in temp){
		if (sum(match(sampled,j,0))==0 || j==-1) {
			# replace all -j values in edge by interior
			posvalue <- interior
			interior <- interior +1
		} else {
			posvalue <- leaf
			leaf <- leaf +1
		}
		replacel <- which(edge == j)
		if (length(replacel)>0)  {
			for (k in 1:length(replacel)) {
				if ((replacel[k]-1) < length(edge.length)) {
					edge[replacel[k],1] <- posvalue
				} else {
					edge[(replacel[k]-length(edge.length)),2] <- posvalue
				}
			}
		}
	}



	phy <- list(edge = edge)
	phy$tip.label <- paste("t", sample(length(sampled)), sep = "")
	phy$edge.length <- edge.length
	phy$Nnode <- length(sampled)
	class(phy) <- "phylo"
	phy2 <- phy} else {print("no sampled extant or extinct individuals after time age")
		phy2<-0
		}
if (detail==1){
phy2<-c(phy2,extant)}
phy2
}
