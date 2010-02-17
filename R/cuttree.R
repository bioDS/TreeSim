cuttree <-
function(tree,cuttime){
	branching <- branching.times.complete(tree)
	ages<-age.tips(tree)
	deletetip <- which(ages[,2] < cuttime)
	while (length(deletetip)>0){
		i <- length(deletetip)   #we delete from highest to lowest tipname
		if (length(deletetip)>1 && ages[deletetip[i],2]==0){
			i<-i-1
			}
		parent <- tree$edge[,1][tree$edge[,2]==deletetip[i]]
		if (branching[as.character(parent)] < cuttime){
			tree <- drop.tip(tree,deletetip[i])
		} else {
			tree$edge.length[which(tree$edge[,2] == deletetip[i])] = branching[as.character(parent)]-cuttime
		}
		if (length(deletetip)>1){
			branching <- branching.times.complete(tree)
			ages<-age.tips(tree)
			deletetip <- which(ages[,2] < (cuttime-0.0000001))
			}  else {
			deletetip <- vector()
			}
	}
	tree
	}

