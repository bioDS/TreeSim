getx<-function(datatree,sersampling=0){
    datatree$node.label<-NULL	
	if (sersampling==1){x<-get.times.polytomy(datatree)} else {
	br<-branching.times(datatree)
	edges<-datatree$edge
	edges1<-edges[,1]
	edges2<-edges[,2]
	ord<-order(edges1)
	edges2<-cbind(edges1[ord],edges2[ord])
	br2<-vector()
	temp<-edges[1,1]
	index<-1
	for (j in 2:length(edges2[,1])){
		if (edges2[j,1] == temp){
		index <- index+1
		label<-paste(edges2[j,1],sep="")
		br2<-c(br2,br[label])
		} else {
		if (index==1){
		print("wierd")
		break
		}
		index<-1
		temp<-edges2[j,1]
		}
	}
	x<-sort(br2)}
	x
}