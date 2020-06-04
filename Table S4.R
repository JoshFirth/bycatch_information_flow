rm(list=ls())
library(igraph)
a<-load(file="Data/Raw Data Objects.RData")

display.contexts<-c("Gear","Weather","Location","Activity","T. Bycatch", "Tech.", "Regs.","Finance","Capt.")
short.contexts<-short.names[4:12]
short.contexts.t1<-c("turtle",short.contexts[!short.contexts=="turtle"])
display.contexts.t1<-c("T. Bycatch",display.contexts[!display.contexts=="T. Bycatch"])



display.contexts<-c("Gear","Weather","Location","Activity","T. Bycatch", "Tech.", "Regs.","Finance","Capt.")
short.contexts<-short.names[4:12]
short.contexts.t1<-c("turtle",short.contexts[!short.contexts=="turtle"])
display.contexts.t1<-c("T. Bycatch",display.contexts[!display.contexts=="T. Bycatch"])



#Depending on which permutations needed
#EDGE PERMS
p1<-load(paste0("Data/context perms 1.RData"))  
p2<-load(paste0("Data/context perms 2.RData")) 


#Table for the context permutations
type.stats<-c("Obs.","Context P1 - Mean","Context P1 - SD","Context P1 - LR","Context P1 - UR","Context P1 - Pval","Context P2 - Mean","Context P2 - SD","Context P2 - LR","Context P2 - UR","Context P2 - Pval")

supp.context.stats<-c("V. Eccent.","Avg. Eccent.")

supp.context.tab<-matrix(NA,nrow=(length(short.contexts.t1)*length(supp.context.stats)),ncol=length(type.stats),dimnames=list(rep(short.contexts.t1,length(supp.context.stats)),type.stats))

#need a new v of the function for context perm
report.contextp.stats<-function(net.function){ #just define the function to use
netstat<-sapply(resp.just.infos.i,function(a)net.function(a)) #a turtle lowest inc resp only
netstat.p1<-sapply(resp.just.infos.p1.i,function(b) sapply(b, function(a)net.function(a)),simplify=T) 
netstat.p1<-t(netstat.p1)
netstat.p2<-sapply(resp.just.infos.p2.i,function(b) sapply(b, function(a)net.function(a)),simplify=T) 
netstat.p2<-t(netstat.p2)
for.vals.plt.p1<-netstat.p1 #assign the first perm type values (a matrix)
for.vals.plt.p2<-netstat.p2 #assign the second perm type values (a matrix)
for.vals.plt.obs<-netstat #assign the observed values (use a vector of the points)
colnames(for.vals.plt.p1)<-short.contexts
vals.plt.p1<-for.vals.plt.p1[,short.contexts.t1]
colnames(for.vals.plt.p2)<-short.contexts
vals.plt.p2<-for.vals.plt.p2[,short.contexts.t1]
names(for.vals.plt.obs)<-short.contexts
vals.plt.obs<-for.vals.plt.obs[short.contexts.t1]
p1.mean<-apply(vals.plt.p1,2,mean)
p1.sd<-apply(vals.plt.p1,2,sd)
p1.lq<-apply(vals.plt.p1,2,quantile,probs=0.025)
p1.uq<-apply(vals.plt.p1,2,quantile,probs=0.975)
p1.p<-mapply(function(a,b)perm.p(a,b),a=as.list(vals.plt.obs),lapply(apply(vals.plt.p1,2,list),function(d)d[[1]]))
p2.mean<-apply(vals.plt.p2,2,mean)
p2.sd<-apply(vals.plt.p2,2,sd)
p2.lq<-apply(vals.plt.p2,2,quantile,probs=0.025)
p2.uq<-apply(vals.plt.p2,2,quantile,probs=0.975)
p2.p<-mapply(function(a,b)perm.p(a,b),a=as.list(vals.plt.obs),lapply(apply(vals.plt.p2,2,list),function(d)d[[1]]))
full.info<-cbind(vals.plt.obs,p1.mean,p1.sd,p1.lq,p1.uq,p1.p,p2.mean,p2.sd,p2.lq,p2.uq,p2.p)
full.info
}

v.eccent.context<-report.contextp.stats(function(a){var(eccentricity(a,mode="in"),na.rm=T)})
m.eccent.context<-report.contextp.stats(function(a){mean(eccentricity(a,mode="in"),na.rm=T)})
supp.context.tab.vals<-rbind(v.eccent.context,m.eccent.context)
rownames(supp.context.tab.vals)<-rownames(supp.context.tab.vals)
colnames(supp.context.tab.vals)<-colnames(supp.context.tab.vals)



#Format table for text print out

sctv<-as.data.frame(supp.context.tab.vals)
sctv$stat<-rep(c("var eccent","mean eccent"),c(9,9))
sctv$context<-sctv[,1]
sctv$obs1<-as.character(signif(round(sctv[,2],4),4))
sctv$mean.sd.1<-paste0(signif(round(sctv[,3],4),4)," (",signif(round(sctv[,4],4),4),")")
sctv$lq.uq.1<-paste0(signif(round(sctv[,5],4),4)," to ",signif(round(sctv[,6],4),4))
sctv$p1<-sctv[,7]
sctv$mean.sd.2<-paste0(signif(round(sctv[,8],4),4)," (",signif(round(sctv[,9],4),4),")")
sctv$lq.uq.2<-paste0(signif(round(sctv[,10],4),4)," to ",signif(round(sctv[,11],4),4))
sctv$p2<-sctv[,12]
sctv.c<-sctv[,13:ncol(sctv)]

table.s4<-sctv.c


table.s4 #Formatted version

