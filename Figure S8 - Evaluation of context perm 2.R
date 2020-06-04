
rm(list=ls())
library(igraph)
a<-load(file="Data/Raw Data Objects.RData")

display.contexts<-c("Gear","Weather","Location","Activity","T. Bycatch", "Tech.", "Regs.","Finance","Capt.")
short.contexts<-short.names[4:12]
short.contexts.t1<-c("turtle",short.contexts[!short.contexts=="turtle"])
display.contexts.t1<-c("T. Bycatch",display.contexts[!display.contexts=="T. Bycatch"])

resp.am.a.blank<-matrix(0,nrow(resp.just.infos[[1]]),ncol(resp.just.infos[[1]]),dimnames=list(rownames(resp.just.infos[[1]]),colnames(resp.just.infos[[1]]))) #am.a is going to be all individuals


resp.am.a.info.type.l.p2<-rep(list(resp.am.a.blank),length(info.types)) #create a list of blank matrices for each info type (about to be filled) 
names(resp.am.a.info.type.l.p2)<-info.types
#replicate this list lots of times
resp.am.a.info.type.l.p2<-rep(list(resp.am.a.info.type.l.p2),nrands)
resp.dat.p2<-resp.dat #make a null resp.database
resp.dat.mat<-resp.dat[,info.types]
resp.dat.mat[resp.dat.mat=="2"]<-"0"
resp.dat.mat[1:length(resp.dat.mat)]<-ifelse(resp.dat.mat=="1",1,0)
resp.dat.mat.p2<-resp.dat.mat
mean.same<-NA
for(j in 1:12000){ #remember 
resp.dat.mat.p2<-gbi.perm(resp.dat.mat.p2,1) #j multiplied by this is number of swaps - so 10,000 at 10 = 100,000 swaps
mean.same[j]<-mean(resp.dat.mat.p2==resp.dat.mat)
print(j)
}
dev.new(height=5,width=6)
par(mar=c(5,4,1,1))
plot(mean.same,type="l",ylab="Proportion of links the same",xlab="Number of swaps",bty="n",ylim=c(0.8,1))
used.range<-seq(2000,12000,by=100)
abline(v=min(used.range),col="blue")
seg.length<-0.01
segments(used.range,mean.same[used.range]+seg.length,used.range,mean.same[used.range]-seg.length,col="darkblue",pch="|",lwd=0.2)
segments(used.range,mean.same[used.range]+seg.length,used.range,mean.same[used.range]-seg.length,col="darkblue",pch="|",lwd=0.2)

