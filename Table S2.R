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
main.stats<-c("Assortment","V. Eccent.")

main.tab<-matrix(NA,nrow=(length(short.contexts.t1)*length(main.stats)),ncol=length(type.stats),dimnames=list(rep(short.contexts.t1,length(main.stats)),type.stats))




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
#####MAKE THE MAIN TABLE (SUPP INFO TABLE S2)
in.assort<-report.edgep.stats(function(a){assortativity(a,degree(a,mode="in"))})
v.eccent<-report.edgep.stats(function(a){var(eccentricity(a,mode="in"),na.rm=T)})
main.tab.vals<-rbind(in.assort,v.eccent)
rownames(main.tab.vals)<-rownames(main.tab)
colnames(main.tab.vals)<-colnames(main.tab)


main.tab.vals #TABLE S2 - Not formatted


#Formatting tables as text like objects
mtv<-as.data.frame(main.tab.vals)
mtv$stat<-rep(c("in assort","var eccent"),c(9,9))

mtv$context<-mtv[,1]
mtv$obs1<-as.character(signif(round(mtv[,2],4),4))
mtv$mean.sd.1<-paste0(signif(round(mtv[,3],4),4)," (",signif(round(mtv[,4],4),4),")")
mtv$lq.uq.1<-paste0(signif(round(mtv[,5],4),4)," to ",signif(round(mtv[,6],4),4))
mtv$p1<-mtv[,7]
mtv$mean.sd.2<-paste0(signif(round(mtv[,8],4),4)," (",signif(round(mtv[,9],4),4),")")
mtv$lq.uq.2<-paste0(signif(round(mtv[,10],4),4)," to ",signif(round(mtv[,11],4),4))
mtv$p2<-mtv[,12]
mtv.c<-mtv[,13:ncol(mtv)]

table.s2<-mtv

table.s2 #formatted version









