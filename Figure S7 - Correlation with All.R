
rm(list=ls())
library(igraph)
a<-load(file="Data/Raw Data Objects.RData")

display.contexts<-c("Gear","Weather","Location","Activity","T. Bycatch", "Tech.", "Regs.","Finance","Capt.")
short.contexts<-short.names[4:12]
short.contexts.t1<-c("turtle",short.contexts[!short.contexts=="turtle"])
display.contexts.t1<-c("T. Bycatch",display.contexts[!display.contexts=="T. Bycatch"])


res.mat<-sapply(resp.just.infos,function(a)sapply(resp.just.infos,function(b)cor(as.numeric(a),as.numeric(b))))
#create overall results matrix
short.contexts<-short.names[4:12]
rownames(res.mat)<-colnames(res.mat)<-short.contexts
res.info.type.cors<-res.mat

resp.am.a.blank<-matrix(0,nrow(resp.just.infos[[1]]),ncol(resp.just.infos[[1]]),dimnames=list(rownames(resp.just.infos[[1]]),colnames(resp.just.infos[[1]]))) #am.a is going to be all individuals
resp.am.a.any<-resp.am.a.blank #if they were simply named as a nominee or not (directed matrix)
resp.am.a.any[cbind(resp.dat$resp.id.c,resp.dat$noms.id.c)]<-1 
res.info.any.cors<-sapply(resp.just.infos,function(a)cor(as.numeric(a),as.numeric(resp.am.a.any)))
names(res.info.any.cors)<-short.contexts



for.any.cor.obs<-res.info.any.cors
any.cor.obs<-for.any.cor.obs[short.contexts.t1]


###################################################
cols.d<-"black"
dev.new(height=5.5,width=8.7);par(mar=c(4,4,1,0.2))
plot(any.cor.obs~c(1:length(any.cor.obs)),xlim=c(0.5,length(any.cor.obs)+0.5),ylim=range(0.75,1),cex=2,pch=16,col=cols.d,xaxt="n",main="",xlab="",ylab="",type="n",bty="n") #,yaxt="n"

axis(side=1,at=c(1:length(any.cor.obs)),labels=display.contexts.t1,tcl=-0.4,cex.axis=1.0)
#axis(side=2,at=seq(0,0.7,by=0.1),mgp=c(1.5, 0.6, 0),las=1,tcl=-0.4)
mtext("Any Nomination Network Corr.",side=2,line=2.2,adj=0.5,outer=F) #whatever the y axis is
#Add bootstrap
res.info.any.cors.bs<-sapply(resp.just.infos,function(a)bcor(x=as.numeric(a),y=as.numeric(resp.am.a.any),1000,F))
colnames(res.info.any.cors.bs)<-short.contexts
for.any.cor.obs.bs<-res.info.any.cors.bs
any.cor.obs.bs<-for.any.cor.obs.bs[,short.contexts.t1]
uq<-apply(any.cor.obs.bs,2,quantile,probs=0.975)
lq<-apply(any.cor.obs.bs,2,quantile,probs=0.025)
segments(1:length(any.cor.obs),uq,1:length(any.cor.obs),lq,col="darkgrey")
#add points
points(any.cor.obs,pch=20,col="red",cex=1.5)