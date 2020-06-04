
#Correlation w/ context perms

rm(list=ls())
library(igraph)
a<-load(file="Data/Raw Data Objects.RData")

display.contexts<-c("Gear","Weather","Location","Activity","T. Bycatch", "Tech.", "Regs.","Finance","Capt.")

res.mat<-sapply(resp.just.infos,function(a)sapply(resp.just.infos,function(b)cor(as.numeric(a),as.numeric(b))))
#create overall results matrix
short.contexts<-short.names[4:12]
rownames(res.mat)<-colnames(res.mat)<-short.contexts
res.info.type.cors<-res.mat

p3<-load(paste0("Data/edge perms 1.RData"))  
p4<-load(paste0("Data/edge perms 2.RData")) 


#Storage
res.info.type.cors.p3<-res.info.type.cors.p4<-rep(list(matrix(NA,length(short.contexts),length(short.contexts),dimnames=list(short.contexts,short.contexts))),nrands)
for(i in 1:nrands){
res.mat.p3<-sapply(resp.just.infos.p3[[i]],function(a)sapply(resp.just.infos.p3[[i]],function(b)cor(as.numeric(a),as.numeric(b))))
rownames(res.mat.p3)<-colnames(res.mat.p3)<-short.contexts
res.info.type.cors.p3[[i]]<-res.mat.p3

res.mat.p4<-sapply(resp.just.infos.p4[[i]],function(a)sapply(resp.just.infos.p4[[i]],function(b)cor(as.numeric(a),as.numeric(b))))
rownames(res.mat.p4)<-colnames(res.mat.p4)<-short.contexts
res.info.type.cors.p4[[i]]<-res.mat.p4
print(i)
}

rel.col.names<-colnames(res.info.type.cors)[col(res.info.type.cors)[lower.tri(res.info.type.cors)]]
rel.row.names<-rownames(res.info.type.cors)[row(res.info.type.cors)[lower.tri(res.info.type.cors)]]
cor.vs<-paste(rel.col.names,rel.row.names,sep="_")


unfold.cor.df.p3<-t(sapply(res.info.type.cors.p3,function(a)a[lower.tri(a)]))
unfold.cor.df.p4<-t(sapply(res.info.type.cors.p4,function(a)a[lower.tri(a)]))
colnames(unfold.cor.df.p3)<-colnames(unfold.cor.df.p4)<-cor.vs

unfold.cor.df<-res.info.type.cors[lower.tri(res.info.type.cors)]
names(unfold.cor.df)<-cor.vs


#Plotting

n.obs<-1 #what to divide the freq distributions by (only if wanting to calculate proportions)
quantile.range <-c(0.025,0.975)#sets the quantiles, can do as 0 and 1 if wanting full

#First calculate distribution for permutation 1:
measures<-colnames(unfold.cor.df.p3)
unfold.cor.df.p3.means<-apply(unfold.cor.df.p3,2,mean,na.rm=T)
	#Distribution function:
d.all3<-apply(unfold.cor.df.p3,2,function(vals){
vals.c<-vals/n.obs #corrects for number of obs if wanting to do freq dist
qs<-quantile(vals.c,probs=quantile.range,na.rm=T)
min.qs<-min(qs);max.qs<-max(qs)
e<-density(vals.c,na.rm=T)[c("x","y")]
x.qs<-c(min.qs,e$x[e$x>=min.qs & e$x<=max.qs],max.qs)
y.qs<-c(0,e$y[e$x>=min.qs & e$x<=max.qs],0)
e<-c(e,list(x.qs,y.qs))
names(e)<-c("x.all","y.all","x.qs","y.qs")
e})
names(d.all3)<-measures

measures<-colnames(unfold.cor.df.p4)
unfold.cor.df.p4.means<-apply(unfold.cor.df.p4,2,mean,na.rm=T)
d.all4<-apply(unfold.cor.df.p4,2,function(vals){
vals.c<-vals/n.obs #corrects for number of obs if wanting to do freq dist
qs<-quantile(vals.c,probs=quantile.range,na.rm=T) #sets the quantiles, can do as 0 and 1 if wanting full
min.qs<-min(qs);max.qs<-max(qs)
e<-density(vals.c,na.rm=T)[c("x","y")]
x.qs<-c(min.qs,e$x[e$x>=min.qs & e$x<=max.qs],max.qs)
y.qs<-c(0,e$y[e$x>=min.qs & e$x<=max.qs],0)
e<-c(e,list(x.qs,y.qs))
names(e)<-c("x.all","y.all","x.qs","y.qs")
e})
names(d.all4)<-measures


cols.d<-rep("seagreen3",length(measures))
cols.l<-rep("seagreen1",length(measures))
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
par(mar=c(0.5,2.7,0.5,0.2))

obs.u<-unfold.cor.df[m]
measure.u<-measures[m]
panel.rows.u<-row(plot.panels)[plot.panels==measure.u]
panel.cols.u<-col(plot.panels)[plot.panels==measure.u]
	#Get distribution
using3<-d.all3[[measure.u]]
using4<-d.all4[[measure.u]]


ylims.exact<-range(c(obs.u,using3$x.qs,using4$x.qs))
ylims.round<-c(floor(ylims.exact[1]*100),ceiling(ylims.exact[2]*100))/100
perms.lim<-range(c(using3$x.qs,using4$x.qs))

par(mfg=c(panel.rows.u[1],panel.cols.u[1]))
plot(obs.u~1,xlim=c(0.5,1.5),ylim=ylims.round,xaxt="n",yaxt="n",main="",xlab="",ylab="",type="n",bty="n") #,yaxt="n"

#axis(side=1,at=1),labels=measure.u,tcl=-0.4,cex.axis=1.0)
axis(side=2,at=ylims.round,labels=ylims.round,mgp=c(1.5, 0.6, 0),las=1,tcl=-0.4,cex.axis=1.1)
#mtext("Corr.",side=2,line=2.2,adj=0.5,outer=F) #whatever the y axis is

mag<-0.2/max(c(using3$y.qs,using4$y.qs)) 
actual.stretch<-diff(ylims.round)/diff(perms.lim)
actual.stretch2<-(actual.stretch^2)/1.5

gap<-0.010 #gap is the distance between the left histogram and the right histogram



polygon(1-gap-range.use(using3$y.qs,0,0.11*actual.stretch2),using3$x.qs,col=makeTrans(cols.d[measure.u],alpha=1),border=NA)
polygon(1+gap+range.use(using4$y.qs,0,0.11*actual.stretch2),using4$x.qs,col=makeTrans(cols.l[measure.u],alpha=1),border=NA)

seg.l<-0.35 #how far to draw a line along
obs.l.col<-ifelse(obs.u<perms.lim[1] | obs.u>perms.lim[2],"red","black")
segments(1-seg.l,obs.u,1+seg.l,obs.u,lwd=2,col=obs.l.col)

#Now print result in other panel
par(mfg=c(panel.rows.u[2],panel.cols.u[2]))
par(mar=c(0,0,0,0))
plot(1:10,1:10,xaxt="n",yaxt="n",main="",xlab="",ylab="",type="n",bty="n")
Perm.result3<-paste0("",paste((round(range(using3$x.qs),3)),collapse="-"))
Perm.result4<-paste0("",paste((round(range(using4$x.qs),3)),collapse="-"))
Obs.result<-as.numeric(round(obs.u,4))

text(5,7,Obs.result,cex=1.7,col=obs.l.col)
text(5,5,Perm.result3,cex=1.25,col="black")
text(5,3,Perm.result4,cex=1.2,col="black")
}




