
rm(list=ls())
library(igraph)
a<-load(file="Data/Raw Data Objects.RData")

resp.am.a.blank<-matrix(0,nrow(resp.just.infos[[1]]),ncol(resp.just.infos[[1]]),dimnames=list(rownames(resp.just.infos[[1]]),colnames(resp.just.infos[[1]]))) #am.a is going to be all individuals

types.to.do<-c("1","2","3","4") #1 and 2 are context perms, 3 and 4 are network perms

respondents.only<-T
clean.as.go<-T #whether to clean the WS as you run the models

#1 - specifically controlling for the likelihood that each context is nominated i.e. under the given probabilities of each context nomination.
#storage
if("1" %in% types.to.do){
resp.am.a.info.type.l.p1<-rep(list(resp.am.a.blank),length(info.types)) #create a list of blank matrices for each info type (about to be filled) 
names(resp.am.a.info.type.l.p1)<-info.types
#replicate this list lots of times
resp.am.a.info.type.l.p1<-rep(list(resp.am.a.info.type.l.p1),nrands)
resp.dat.p1<-resp.dat #make a null resp.database
for(i in 1:nrands){
#randomly assign the context of the link (with given probability of each context)
for(j in 1:length(info.types)){resp.dat.p1[,info.types][,j]<-sample(resp.dat.p1[,info.types][,j])}
yes.info.type.p1<-lapply(info.types,function(a)(resp.dat.p1[,a])=="1")
resp.am.a.info.type.l.p1[[i]]<-mapply(function(am,yes){am[((cbind(resp.dat.p1$resp.id.c,resp.dat.p1$noms.id.c)[yes,]))]<-1 #any dyads which said yes to that info type are marked
	am}, #returns am
	am=resp.am.a.info.type.l.p1[[i]],yes=yes.info.type.p1,SIMPLIFY=F)
print(i)

}
resp.just.infos.p1<-resp.am.a.info.type.l.p1
resp.just.infos.p1.i<-sapply(resp.just.infos.p1,function(b) sapply(b, function(a)graph.adjacency(a,"directed",T,diag=F),simplify=F),simplify=F)

save(resp.just.infos.p1,resp.just.infos.p1.i,file="Data/context perms 1.RData")

}#END '1' TYPES TO DO


if(clean.as.go==T){rm(list=c("resp.am.a.info.type.l.p1","resp.just.infos.p1","resp.just.infos.p1.i"))}


#(2) keeping the dyad part the same, at least you can never get a situation where that dyad doesn't nominate on any context, and you also get the same in each context. This is a GBI permutation, where the contexts are the individuals and the nominations are the groups.
if("2" %in% types.to.do){
resp.am.a.info.type.l.p2<-rep(list(resp.am.a.blank),length(info.types)) #create a list of blank matrices for each info type (about to be filled) 
names(resp.am.a.info.type.l.p2)<-info.types
#replicate this list lots of times
resp.am.a.info.type.l.p2<-rep(list(resp.am.a.info.type.l.p2),nrands)
resp.dat.p2<-resp.dat #make a null resp.database
resp.dat.mat<-resp.dat[,info.types]
resp.dat.mat[resp.dat.mat=="2"]<-"0"
resp.dat.mat[1:length(resp.dat.mat)]<-ifelse(resp.dat.mat=="1",1,0)
resp.dat.mat.p2<-resp.dat.mat
#Burn in
#See 'analsis figures' for working out burn in period and plotting results
resp.dat.mat.p2<-gbi.perm(resp.dat.mat.p2,2000) 
for(i in 1:nrands){
resp.dat.mat.p2<-gbi.perm(resp.dat.mat.p2,100) 
resp.dat.p2[,info.types]<-resp.dat.mat.p2
yes.info.type.p2<-lapply(info.types,function(a)(sample(resp.dat.p2[,a]))==1)
resp.am.a.info.type.l.p2[[i]]<-mapply(function(am,yes){am[((cbind(resp.dat.p2$resp.id.c,resp.dat.p2$noms.id.c)[yes,]))]<-1 #any dyads which said yes to that info type are marked
	am}, #returns am
	am=resp.am.a.info.type.l.p2[[i]],yes=yes.info.type.p2,SIMPLIFY=F)
print(i)
}
resp.just.infos.p2<-resp.am.a.info.type.l.p2
resp.just.infos.p2.i<-sapply(resp.just.infos.p2,function(b) sapply(b, function(a)graph.adjacency(a,"directed",T,diag=F),simplify=F),simplify=F)

save(resp.just.infos.p2,resp.just.infos.p2.i,file="Data/context perms 2.RData")

} #END '2' TYPES TO DO

if(clean.as.go==T){rm(list=c("resp.am.a.info.type.l.p2","resp.just.infos.p2","resp.just.infos.p2.i"))}


#(3) In-Edge permutation. Reassign outgoing edges to an individual who had at least one incoming edge. Maintains outdegree, randomizes incoming edges.
if("3" %in% types.to.do){
#storage
resp.am.a.info.type.l.p3<-rep(list(resp.just.infos),nrands)

for(i in 1:nrands){
resp.am.a.info.type.l.p3[[i]]<-lapply(resp.am.a.info.type.l.p3[[i]],function(a){
	for(rw in 1:nrow(a)){a[rw,][-rw]<-sample(a[rw,][-rw])}
	a})
	
print(i)
}

resp.just.infos.p3<-resp.am.a.info.type.l.p3
resp.just.infos.p3.i<-sapply(resp.just.infos.p3,function(b) sapply(b, function(a)graph.adjacency(a,"directed",T,diag=F),simplify=F),simplify=F)


save(resp.just.infos.p3,resp.just.infos.p3.i,file="Data/edge perms 1.RData")

}#END '3' TYPES TO DO

if(clean.as.go==T){rm(list=c("resp.am.a.info.type.l.p3","resp.just.infos.p3","resp.just.infos.p3.i"))}



#(4) Conservative Edge permutation. Swaps edges. Maintains outdegree and indegree.

if("4" %in% types.to.do){
#storage
resp.am.a.info.type.l.p4<-rep(list(resp.just.infos),nrands)

resp.just.infos.p4<-resp.am.a.info.type.l.p4
resp.just.infos.p4.i<-sapply(resp.just.infos.p4,function(b) sapply(b, function(a)graph.adjacency(a,"directed",T,diag=F),simplify=F),simplify=F)

for(i in 1:nrands){
resp.just.infos.p4.i[[i]]<-lapply(resp.just.infos.p4.i[[i]],function(a)rewire(a,keeping_degseq(niter=ecount(a))))
resp.am.a.info.type.l.p4[[i]]<-resp.just.infos.p4[[i]]<-lapply(resp.just.infos.p4.i[[i]],function(a)as.matrix(as_adjacency_matrix(a)))
print(i)
}


save(resp.just.infos.p4,resp.just.infos.p4.i,file="Data/edge perms 2.RData")

}#END '4' TYPES TO DO


if(clean.as.go==T){rm(list=c("resp.am.a.info.type.l.p4","resp.just.infos.p4","resp.just.infos.p4.i"))}



