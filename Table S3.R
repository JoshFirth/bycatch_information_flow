rm(list=ls())
library(igraph)
a<-load(file="Data/Raw Data Objects.RData")

display.contexts<-c("Gear","Weather","Location","Activity","T. Bycatch", "Tech.", "Regs.","Finance","Capt.")
short.contexts<-short.names[4:12]
short.contexts.t1<-c("turtle",short.contexts[!short.contexts=="turtle"])
display.contexts.t1<-c("T. Bycatch",display.contexts[!display.contexts=="T. Bycatch"])



#Depending on which permutations needed
#EDGE PERMS
p3<-load(paste0("Data/edge perms 1.RData"))  
p4<-load(paste0("Data/edge perms 2.RData")) 



#Table for the edge permutations
type.stats<-c("Obs.","Edge P1 - Mean","Edge P1 - SD","Edge P1 - LR","Edge P1 - UR","Edge P1 - Pval","Edge P2 - Mean","Edge P2 - SD","Edge P2 - LR","Edge P2 - UR","Edge P2 - Pval")
supp.stats<-c("Out-Assort.","V. Bet.","Avg. Eccent.")


supp.tab<-matrix(NA,nrow=(length(short.contexts.t1)*length(supp.stats)),ncol=length(type.stats),dimnames=list(rep(short.contexts.t1,length(supp.stats)),type.stats))


#Make function to report table stats back:
report.edgep.stats<-function(net.function){ #just define the function to use

netstat<-sapply(resp.just.infos.i,function(a)net.function(a)) #a turtle lowest inc resp only
netstat.p3<-sapply(resp.just.infos.p3.i,function(b) sapply(b, function(a)net.function(a)),simplify=T) 
netstat.p3<-t(netstat.p3)
netstat.p4<-sapply(resp.just.infos.p4.i,function(b) sapply(b, function(a)net.function(a)),simplify=T) 
netstat.p4<-t(netstat.p4)

#From here - make objects for generic scripting
for.vals.plt.p3<-netstat.p3 #assign the first perm type values (a matrix)
for.vals.plt.p4<-netstat.p4 #assign the second perm type values (a matrix)
for.vals.plt.obs<-netstat #assign the observed values (use a vector of the points)
colnames(for.vals.plt.p3)<-short.contexts
vals.plt.p3<-for.vals.plt.p3[,short.contexts.t1]
colnames(for.vals.plt.p4)<-short.contexts
vals.plt.p4<-for.vals.plt.p4[,short.contexts.t1]
names(for.vals.plt.obs)<-short.contexts
vals.plt.obs<-for.vals.plt.obs[short.contexts.t1]
p3.mean<-apply(vals.plt.p3,2,mean)
p3.sd<-apply(vals.plt.p3,2,sd)
p3.lq<-apply(vals.plt.p3,2,quantile,probs=0.025)
p3.uq<-apply(vals.plt.p3,2,quantile,probs=0.975)
p3.p<-mapply(function(a,b)perm.p(a,b),a=as.list(vals.plt.obs),lapply(apply(vals.plt.p3,2,list),function(d)d[[1]]))

p4.mean<-apply(vals.plt.p4,2,mean)
p4.sd<-apply(vals.plt.p4,2,sd)
p4.lq<-apply(vals.plt.p4,2,quantile,probs=0.025)
p4.uq<-apply(vals.plt.p4,2,quantile,probs=0.975)
p4.p<-mapply(function(a,b)perm.p(a,b),a=as.list(vals.plt.obs),lapply(apply(vals.plt.p4,2,list),function(d)d[[1]]))

full.info<-cbind(vals.plt.obs,p3.mean,p3.sd,p3.lq,p3.uq,p3.p,p4.mean,p4.sd,p4.lq,p4.uq,p4.p)
full.info
}



############################################
#####MAKE THE EDGE PERM SUPP INFO TABLE (SUPP INFO TABLE S3)
#Make function to report table stats back:
report.edgep.stats2<-function(net.function){ #just define the function to use (non-igraph version)
netstat<-sapply(resp.just.infos,function(a)net.function(a)) #a turtle lowest inc resp only
netstat.p3<-sapply(resp.just.infos.p3,function(b) sapply(b, function(a)net.function(a)),simplify=T) 
netstat.p3<-t(netstat.p3)
netstat.p4<-sapply(resp.just.infos.p4,function(b) sapply(b, function(a)net.function(a)),simplify=T) 
#Now same as above function:
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
p3.mean<-apply(vals.plt.p3,2,mean)
p3.sd<-apply(vals.plt.p3,2,sd)
p3.lq<-apply(vals.plt.p3,2,quantile,probs=0.025)
p3.uq<-apply(vals.plt.p3,2,quantile,probs=0.975)
p3.p<-mapply(function(a,b)perm.p(a,b),a=as.list(vals.plt.obs),lapply(apply(vals.plt.p3,2,list),function(d)d[[1]]))
p4.mean<-apply(vals.plt.p4,2,mean)
p4.sd<-apply(vals.plt.p4,2,sd)
p4.lq<-apply(vals.plt.p4,2,quantile,probs=0.025)
p4.uq<-apply(vals.plt.p4,2,quantile,probs=0.975)
p4.p<-mapply(function(a,b)perm.p(a,b),a=as.list(vals.plt.obs),lapply(apply(vals.plt.p4,2,list),function(d)d[[1]]))
full.info<-cbind(vals.plt.obs,p3.mean,p3.sd,p3.lq,p3.uq,p3.p,p4.mean,p4.sd,p4.lq,p4.uq,p4.p)
full.info
}


out.assort<-report.edgep.stats(function(a){assortativity(a,degree(a,mode="out"))})
m.eccent<-report.edgep.stats(function(a){mean(eccentricity(a,mode="in"),na.rm=T)})
unloadNamespace("igraph")
library(sna)
v.bet<-report.edgep.stats2(function(a){var(betweenness(a),na.rm=T)})
unloadNamespace("sna")
library(igraph)
supp.tab.vals<-rbind(out.assort,m.eccent,v.bet)
rownames(supp.tab.vals)<-rownames(supp.tab)
colnames(supp.tab.vals)<-colnames(supp.tab)

supp.tab.vals #Table S3

#Format for text
stv<-as.data.frame(supp.tab.vals)
stv$stat<-rep(c("out assort","mean eccent","var between"),c(9,9,9))
stv$context<-stv[,1]
stv$obs1<-as.character(signif(round(stv[,2],4),4))
stv$mean.sd.1<-paste0(signif(round(stv[,3],4),4)," (",signif(round(stv[,4],4),4),")")
stv$lq.uq.1<-paste0(signif(round(stv[,5],4),4)," to ",signif(round(stv[,6],4),4))
stv$p1<-stv[,7]
stv$mean.sd.2<-paste0(signif(round(stv[,8],4),4)," (",signif(round(stv[,9],4),4),")")
stv$lq.uq.2<-paste0(signif(round(stv[,10],4),4)," to ",signif(round(stv[,11],4),4))
stv$p2<-stv[,12]
stv.c<-stv[,13:ncol(stv)]


table.s3<-stv.c

table.s3












