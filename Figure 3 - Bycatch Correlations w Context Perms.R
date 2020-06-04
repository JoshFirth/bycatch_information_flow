
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

measures<-colnames(unfold.cor.df.p1)

turt.u<-grep("turtle",cor.vs)
measures.turt<-measures[turt.u]
unfold.cor.df.turt<-unfold.cor.df[turt.u]


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



dev.new(height=5.5,width=7.7);par(mar=c(4,4,1,0.2))
plot(unfold.cor.df.turt~c(1:length(measures.turt)),xlim=c(0.5,length(measures.turt)+0.5),ylim=range(0.6,0.8),cex=2,pch=16,col=cols.d,xaxt="n",main="",xlab="",ylab="",type="n",bty="n") #,yaxt="n"

axis(side=1,at=c(1:length(measures.turt)),labels=display.contexts[!display.contexts=="T. Bycatch"],tcl=-0.4,cex.axis=1.0)
#axis(side=2,at=seq(0,0.7,by=0.1),mgp=c(1.5, 0.6, 0),las=1,tcl=-0.4)
mtext("Network Corr.",side=2,line=2.2,adj=0.5,outer=F) #whatever the y axis is

mag<-0.004 #mag is how much to stretch the distributions up and down (i.e. their overall size)
gap<-0.025 #gap is the distance between the left histogram and the right histogram
tot.height<-0.5
for(i in 1:length(measures.turt)){
measure.u<-measures.turt[i]
using1<-d.all1[[measure.u]]
using2<-d.all2[[measure.u]]
obs.u<-unfold.cor.df[measure.u]

#ylims.exact<-range(c(obs.u,using1$x.qs,using2$x.qs))
#ylims.round<-c(floor(ylims.exact[1]*100),ceiling(ylims.exact[2]*100))/100
perms.lim<-range(c(using1$x.qs,using2$x.qs))

total.height1<-using1$y.qs*mag
total.height2<-using2$y.qs*mag
if(max(total.height1)>tot.height){total.height1<-range.use(total.height1,0,tot.height)}
if(max(total.height2)>tot.height){total.height2<-range.use(total.height2,0,tot.height)}

polygon(i-gap-(total.height1),using1$x.qs,col=makeTrans(cols.d,alpha=1),border=NA)
polygon(i+gap+(total.height2),using2$x.qs,col=makeTrans(cols.l,alpha=1),border=NA)

seg.l<-tot.height*0.9 #how far to draw a line along
obs.l.col<-ifelse(obs.u<perms.lim[1] | obs.u>perms.lim[2],"red","black")
segments(i-seg.l,obs.u,i+seg.l,obs.u,lwd=2,col=obs.l.col)

}







