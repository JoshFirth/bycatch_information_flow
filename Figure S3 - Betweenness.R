library(sna)
a<-load(file="Data/Raw Data Objects.RData")

display.contexts<-c("Gear","Weather","Location","Activity","T. Bycatch", "Tech.", "Regs.","Finance","Capt.")
short.contexts<-short.names[4:12]
short.contexts.t1<-c("turtle",short.contexts[!short.contexts=="turtle"])
display.contexts.t1<-c("T. Bycatch",display.contexts[!display.contexts=="T. Bycatch"])

cols.d<-"seagreen3"
cols.l<-"seagreen1"

#Load edge permuted datasets
p3<-load(paste0("Data/edge perms 1.RData"))  
p4<-load(paste0("Data/edge perms 2.RData")) 

net.function<-function(a){var(betweenness(a),na.rm=T)}
netstat<-sapply(resp.just.infos,function(a)net.function(a)) #a turtle lowest inc resp only
netstat.p3<-sapply(resp.just.infos.p3,function(b) sapply(b, function(a)net.function(a)),simplify=T) 
netstat.p3<-t(netstat.p3)
netstat.p4<-sapply(resp.just.infos.p4,function(b) sapply(b, function(a)net.function(a)),simplify=T) 
netstat.p4<-t(netstat.p4)
for.vals.plt.p3<-netstat.p3 #assign the first perm type values (a matrix)
for.vals.plt.p4<-netstat.p4 #assign the second perm type values (a matrix)
for.vals.plt.obs<-netstat #assign the observed values (use a vector of the points)

colnames(for.vals.plt.p3)<-short.contexts
vals.plt.p3<-for.vals.plt.p3[,short.contexts.t1]
colnames(for.vals.plt.p4)<-short.contexts
vals.plt.p4<-for.vals.plt.p4[,short.contexts.t1]
names(for.vals.plt.obs)<-short.contexts
vals.plt.obs<-for.vals.plt.obs[short.contexts.t1]

#Distribution function:
n.obs<-1 #what to divide the freq distributions by (only if wanting to calculate proportions)
quantile.range <-c(0.025,0.975)#sets the quantiles, can do as 0 and 1 if wanting full	
d.all1<-apply(vals.plt.p3,2,function(vals){
vals.c<-vals/n.obs #corrects for number of obs if wanting to do freq dist
qs<-quantile(vals.c,probs=quantile.range,na.rm=T)
min.qs<-min(qs);max.qs<-max(qs)
e<-density(vals.c,na.rm=T)[c("x","y")]
x.qs<-c(min.qs,e$x[e$x>=min.qs & e$x<=max.qs],max.qs)
y.qs<-c(0,e$y[e$x>=min.qs & e$x<=max.qs],0)
e<-c(e,list(x.qs,y.qs))
names(e)<-c("x.all","y.all","x.qs","y.qs")
e})

d.all2<-apply(vals.plt.p4,2,function(vals){
vals.c<-vals/n.obs #corrects for number of obs if wanting to do freq dist
qs<-quantile(vals.c,probs=quantile.range,na.rm=T) #sets the quantiles, can do as 0 and 1 if wanting full
min.qs<-min(qs);max.qs<-max(qs)
e<-density(vals.c,na.rm=T)[c("x","y")]
x.qs<-c(min.qs,e$x[e$x>=min.qs & e$x<=max.qs],max.qs)
y.qs<-c(0,e$y[e$x>=min.qs & e$x<=max.qs],0)
e<-c(e,list(x.qs,y.qs))
names(e)<-c("x.all","y.all","x.qs","y.qs")
e})



dev.new(height=5.5,width=8.7);par(mar=c(4,4,1,0.2))
yrange.def<-range(vals.plt.obs,unlist(sapply(d.all1,function(a)a$x.qs)),unlist(sapply(d.all2,function(a)a$x.qs)))


plot(vals.plt.obs~c(1:length(vals.plt.obs)),xlim=c(0.5,length(vals.plt.obs)+0.5),cex=2,pch=16,col=cols.d,xaxt="n",main="",xlab="",ylab="",type="n",bty="n",ylim=c(yrange.def)) #,yaxt="n" ,ylim=range(-0.1,1)

axis(side=1,at=c(1:length(vals.plt.obs)),labels=display.contexts.t1,tcl=-0.4,cex.axis=1.0)
mtext("",side=2,line=2.2,adj=0.5,outer=F) 

mag<-200000.5 #mag is how much to stretch the distributions up and down (i.e. their overall size)
gap<-0.025 #gap is the distance between the left histogram and the right histogram
tot.height<-0.3
for(i in 1:length(vals.plt.obs)){
measure.u<-short.contexts.t1[i]
using1<-d.all1[[measure.u]]
using2<-d.all2[[measure.u]]
obs.u<-vals.plt.obs[measure.u]

perms.lim<-range(c(using1$x.qs,using2$x.qs))
perms.lim.A<-range(using1$x.qs)
perms.lim.B<-range(using2$x.qs)
total.height1<-using1$y.qs*mag
total.height2<-using2$y.qs*mag
if(max(total.height1)>tot.height){total.height1<-range.use(total.height1,0,tot.height)}
if(max(total.height2)>tot.height){total.height2<-range.use(total.height2,0,tot.height)}

polygon(i-gap-(total.height1),using1$x.qs,col=makeTrans(cols.d,alpha=1),border=NA)
polygon(i+gap+(total.height2),using2$x.qs,col=makeTrans(cols.l,alpha=1),border=NA)

seg.l<-tot.height*0.8 #how far to draw a line along
obs.l.col.A<-ifelse(obs.u>perms.lim.A[2],"red","black")
obs.l.col.A<-ifelse(obs.u<perms.lim.A[1],"purple",obs.l.col.A)
segments(i-seg.l,obs.u,i,obs.u,lwd=2,col=obs.l.col.A)
obs.l.col.B<-ifelse(obs.u>perms.lim.B[2],"red","black")
obs.l.col.B<-ifelse(obs.u<perms.lim.B[1],"purple",obs.l.col.B)
segments(i,obs.u,i+seg.l,obs.u,lwd=2,col=obs.l.col.B)

}





