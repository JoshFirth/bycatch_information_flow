
#Correlation w/ context perms

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


p1<-load(paste0("Data/context perms 1.RData"))  
p2<-load(paste0("Data/context perms 2.RData")) 


#Storage
res.info.type.cors.p1<-res.info.type.cors.p2<-rep(list(matrix(NA,length(short.contexts),length(short.contexts),dimnames=list(short.contexts,short.contexts))),nrands)
for(i in 1:nrands){
res.mat.p1<-sapply(resp.just.infos.p1[[i]],function(a)sapply(resp.just.infos.p1[[i]],function(b)cor(as.numeric(a),as.numeric(b))))
rownames(res.mat.p1)<-colnames(res.mat.p1)<-short.contexts
res.info.type.cors.p1[[i]]<-res.mat.p1

res.mat.p2<-sapply(resp.just.infos.p2[[i]],function(a)sapply(resp.just.infos.p2[[i]],function(b)cor(as.numeric(a),as.numeric(b))))
rownames(res.mat.p2)<-colnames(res.mat.p2)<-short.contexts
res.info.type.cors.p2[[i]]<-res.mat.p2
print(i)
}


#unfolded database of the correlations
rel.col.names<-colnames(res.info.type.cors)[col(res.info.type.cors)[lower.tri(res.info.type.cors)]]
rel.row.names<-rownames(res.info.type.cors)[row(res.info.type.cors)[lower.tri(res.info.type.cors)]]
cor.vs<-paste(rel.col.names,rel.row.names,sep="_")

unfold.cor.df.p1<-t(sapply(res.info.type.cors.p1,function(a)a[lower.tri(a)]))
unfold.cor.df.p2<-t(sapply(res.info.type.cors.p2,function(a)a[lower.tri(a)]))
colnames(unfold.cor.df.p1)<-colnames(unfold.cor.df.p2)<-cor.vs

unfold.cor.df<-res.info.type.cors[lower.tri(res.info.type.cors)]
names(unfold.cor.df)<-cor.vs

#Plotting

n.obs<-1 #what to divide the freq distributions by (only if wanting to calculate proportions)
quantile.range <-c(0.025,0.975)#sets the quantiles, can do as 0 and 1 if wanting full

#First calculate distribution for permutation 1:
measures<-colnames(unfold.cor.df.p1)
unfold.cor.df.p1.means<-apply(unfold.cor.df.p1,2,mean,na.rm=T)
	#Distribution function:
d.all1<-apply(unfold.cor.df.p1,2,function(vals){
vals.c<-vals/n.obs #corrects for number of obs if wanting to do freq dist
qs<-quantile(vals.c,probs=quantile.range,na.rm=T)
min.qs<-min(qs);max.qs<-max(qs)
e<-density(vals.c,na.rm=T)[c("x","y")]
x.qs<-c(min.qs,e$x[e$x>=min.qs & e$x<=max.qs],max.qs)
y.qs<-c(0,e$y[e$x>=min.qs & e$x<=max.qs],0)
e<-c(e,list(x.qs,y.qs))
names(e)<-c("x.all","y.all","x.qs","y.qs")
e})
names(d.all1)<-measures

measures<-colnames(unfold.cor.df.p2)
unfold.cor.df.p2.means<-apply(unfold.cor.df.p2,2,mean,na.rm=T)
	#Distribution function:
d.all2<-apply(unfold.cor.df.p2,2,function(vals){
vals.c<-vals/n.obs #corrects for number of obs if wanting to do freq dist
qs<-quantile(vals.c,probs=quantile.range,na.rm=T) #sets the quantiles, can do as 0 and 1 if wanting full
min.qs<-min(qs);max.qs<-max(qs)
e<-density(vals.c,na.rm=T)[c("x","y")]
x.qs<-c(min.qs,e$x[e$x>=min.qs & e$x<=max.qs],max.qs)
y.qs<-c(0,e$y[e$x>=min.qs & e$x<=max.qs],0)
e<-c(e,list(x.qs,y.qs))
names(e)<-c("x.all","y.all","x.qs","y.qs")
e})
names(d.all2)<-measures


cols.d<-rep("darkblue",length(measures))
cols.l<-rep("blue",length(measures))
names(cols.d)<-names(cols.l)<-measures

dev.new(height=11.69,width=8.27)
par(mar=c(4,4,1,0.2))
plot.panels<-res.info.type.cors
plot.panels[lower.tri(plot.panels)]<-cor.vs
plot.panels[upper.tri(plot.panels)]<-cor.vs
par(mfrow=dim(plot.panels))
par(mar=c(4,4,1,0.2))
#fill diagonal middle:
for(d in 1:length(diag(plot.panels))){
par(mar=c(0,0,0,0))
par(mfg=c(d,d))
plot(1:10,1:10,xaxt="n",yaxt="n",main="",xlab="",ylab="",type="n")
text(5,5,display.contexts[d],cex=1.5)
}

m<-1
for(m in 1:length(measures)){
par(mar=c(0.5,2.5,0.5,0.2))

obs.u<-unfold.cor.df[m]
measure.u<-measures[m]
panel.rows.u<-row(plot.panels)[plot.panels==measure.u]
panel.cols.u<-col(plot.panels)[plot.panels==measure.u]
	#Get distribution
using1<-d.all1[[measure.u]]
using2<-d.all2[[measure.u]]


ylims.exact<-range(c(obs.u,using1$x.qs,using2$x.qs))
ylims.round<-c(floor(ylims.exact[1]*100),ceiling(ylims.exact[2]*100))/100
perms.lim<-range(c(using1$x.qs,using2$x.qs))

par(mfg=c(panel.rows.u[1],panel.cols.u[1]))
plot(obs.u~1,xlim=c(0.5,1.5),ylim=ylims.round,xaxt="n",yaxt="n",main="",xlab="",ylab="",type="n",bty="n") #,yaxt="n"

axis(side=2,at=ylims.round,labels=paste0(".",ylims.round*100),mgp=c(1.5, 0.6, 0),las=1,tcl=-0.4,cex.axis=1.3)

mag<-0.2/max(c(using1$y.qs,using2$y.qs)) 
actual.stretch<-diff(ylims.round)/diff(perms.lim)
actual.stretch2<-(actual.stretch^2)/1.5
gap<-0.010 #gap is the distance between the left histogram and the right histogram


polygon(1-gap-range.use(using1$y.qs,0,0.11*actual.stretch2),using1$x.qs,col=makeTrans(cols.d[measure.u],alpha=1),border=NA)
polygon(1+gap+range.use(using2$y.qs,0,0.11*actual.stretch2),using2$x.qs,col=makeTrans(cols.l[measure.u],alpha=1),border=NA)

seg.l<-0.35 #how far to draw a line along
obs.l.col<-ifelse(obs.u<perms.lim[1] | obs.u>perms.lim[2],"red","black")
segments(1-seg.l,obs.u,1+seg.l,obs.u,lwd=2,col=obs.l.col)

#Now print result in other panel
par(mfg=c(panel.rows.u[2],panel.cols.u[2]))
par(mar=c(0,0,0,0))
plot(1:10,1:10,xaxt="n",yaxt="n",main="",xlab="",ylab="",type="n",bty="n")
Perm.result1<-paste0("",paste((round(range(using1$x.qs),3)),collapse="-"))
Perm.result2<-paste0("",paste((round(range(using2$x.qs),3)),collapse="-"))
Obs.result<-as.numeric(round(obs.u,4))

text(5,7,Obs.result,cex=1.7,col=obs.l.col)
text(5,5,Perm.result1,cex=1.25,col="black")
text(5,3,Perm.result2,cex=1.2,col="black")
}



