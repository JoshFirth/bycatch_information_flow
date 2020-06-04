#Figure 2b - Network of Bycatch Information

rm(list=ls())
a<-load(file="Data/Raw Data Objects.RData")

library(igraph);library(sp);library(clue);library(fields)



display.contexts<-c("Gear","Weather","Location","Activity","T. Bycatch", "Tech.", "Regs.","Finance","Capt.")
short.contexts<-short.names[4:12]
short.contexts.t1<-c("turtle",short.contexts[!short.contexts=="turtle"])
display.contexts.t1<-c("T. Bycatch",display.contexts[!display.contexts=="T. Bycatch"])


colfunc<- colorRampPalette(c('grey','firebrick1'))

circlay<-T #whether network should be circular
any.lay<-T #whether all networks in loop should have the same layout



dev.new(height=7.5,width=7);par(mar=c(0,0,1,0));par(mfrow=c(1,1))
	
am.u<-resp.just.infos[["nominee_bycatch_turtle_activity"]]
am.i<-graph.adjacency(am.u,mode=("directed"),diag=F,weighted=T) #igraph

any.am.u<-any.am[rownames(am.u),colnames(am.u)]
any.am.i<-graph.adjacency(any.am.u,mode=("directed"),diag=F,weighted=T) #igraph


if(!any.lay){ 
am.lay<-layout_nicely(am.i)}

if(any.lay){
am.lay<-layout_nicely(any.am.i)}

if(circlay==T){ #whether to keep a circle layout

ps<-nrow(am.lay)
dim.tl<-ceiling(sqrt(ps))*2 
xpoints1<-floor(-dim.tl/2):ceiling(dim.tl/2)

xpoints2<-xpoints1+0.5
ypoints1<-xpoints1
ypoints2<-xpoints2
grid.lay1<-expand.grid(xpoints2,ypoints1)
grid.lay2<-expand.grid(xpoints1,ypoints2)
grid.lay3<-expand.grid(xpoints1,ypoints1)
grid.lay4<-expand.grid(xpoints2,ypoints2)
grid.lay<-rbind(grid.lay1,grid.lay2,grid.lay3,grid.lay4)
grid.lay[,1]<-jitter(grid.lay[,1],1)
grid.lay[,2]<-jitter(grid.lay[,2],1)
euc.dist <- function(x1, x2) sqrt(sum((x1 - x2) ^ 2))
grid.lay[,3]<-apply(grid.lay,1,function(a)euc.dist(matrix(c(mean(grid.lay[,1]),mean(grid.lay[,2])),1,2),a))
max.dist<-grid.lay[,3][true.rank(grid.lay[,3])==ps]
g.lay.c<-grid.lay[grid.lay[,3]<=max.dist,]
g.lay.c[,1]<-scale(g.lay.c[,1])[,1]
g.lay.c[,2]<-scale(g.lay.c[,2])[,1]
am.lay[,1]<-scale(am.lay[,1])[,1]
am.lay[,2]<-scale(am.lay[,2])[,1]
distances <- rdist(am.lay[,1:2],g.lay.c[,1:2]) 
sol <- solve_LSAP(t(distances))
am.lay[as.numeric(sol),1:2]<-as.matrix(g.lay.c[,1:2])

}

strengths.out<-strength(am.i,mode="out")
strengths.in<-strength(am.i,mode="in")
vert.sizes<-range01(rank(strengths.in))
vert.sizes<-vert.sizes+0.3
vert.sizes<-vert.sizes*6

vert.cols<-rep("grey",length(vert.sizes))
vert.cols<-as.character(range01(strengths.in))
vert.cols<-colfunc(100)[range.use(rank(as.numeric(cut(as.numeric(strengths.in),breaks = 100))),1,100)]


vert.shapes<-"circle"
vert.cols.trans<-makeTrans(vert.cols)
edge.cols<-makeTrans("red")


#making grey links
edgew<-E(any.am.i)$weight
if(all(edgew %in% 0:1)){
edgew<-1} else{
edgew<-range.use(edgew,0.1,0.5)}
edge.cols<-"grey"
plot.igraph(any.am.i,layout=am.lay,vertex.size=vert.sizes,vertex.label=NA,edge.width=edgew,edge.color=edge.cols,vertex.frame.color=vert.cols.trans,vertex.color=vert.cols.trans,edge.curved=T,add=F,edge.arrow.size=0)

#making red links
edgew<-E(am.i)$weight
if(all(edgew %in% 0:1)){
edgew<-1} else{
edgew<-range.use(edgew,0.1,0.5)}
edge.cols<-makeTrans("red")

plot.igraph(am.i,layout=am.lay,vertex.size=vert.sizes,vertex.label=NA,edge.width=edgew,edge.color=edge.cols,vertex.frame.color=vert.cols.trans,vertex.color=vert.cols.trans,edge.arrow.width=1.8,edge.arrow.size=0.3,edge.curved=T,add=T)


