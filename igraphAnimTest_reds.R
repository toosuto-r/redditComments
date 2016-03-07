#this version of the script has been tested on igraph 1.0.1
#load libraries
library("igraph")
library("RColorBrewer")

#load the edges with time stamp
#there are three columns in edges: id1,id2,time
edges <- read.table("~/R/data/rcomments/subRelationPreGexf_avThresh-re_fullTimeJoin.txt",header=T)

#generate the full graph
g <- graph.data.frame(edges,directed=F)

#generate a cool palette for the graph (darker colors = older nodes)

intPal<-c(rgb(147,23,69, maxColorValue=255),
rgb(142,192,1, maxColorValue=255),
rgb(107,191,195, maxColorValue=255),
rgb(135,52,174, maxColorValue=255),
rgb(247,146,75, maxColorValue=255),
rgb(42,69,104, maxColorValue=255),
rgb(97,71,12, maxColorValue=255),
rgb(223,146,189, maxColorValue=255),
rgb(14,101,67, maxColorValue=255),
rgb(215,37,151, maxColorValue=255),
rgb(86,24,91, maxColorValue=255),
rgb(111,181,244, maxColorValue=255),
rgb(120,36,8, maxColorValue=255),
rgb(246,93,136, maxColorValue=255),
rgb(245,133,119, maxColorValue=255),
rgb(14,128,47, maxColorValue=255),
rgb(83,56,72, maxColorValue=255),
rgb(156,113,17, maxColorValue=255),
rgb(38,158,106, maxColorValue=255),
rgb(169,83,18, maxColorValue=255))

intense.pal <- intPal
#colors for the nodes are chosen from the very beginning

subSizes <- read.table("C:/Users/Ryan/Documents/R/data/rcomments/subSizes.txt",header=TRUE, sep = ",")
totalComm<-sum(subSizes[,2])


# for (p in seq(1,vcount(g))){
#   nodes_size[p]<-log(((subSizes[V(g)$name[p]==as.vector(subSizes[,1]),2])/totalComm)*10000)/50
# }

# nodes_size <- 1

#time in the edges goes from 1 to 300. We kick off at time 3
ti <- 1
#remove edges which are not present
gt <- delete.edges(g,which(E(g)$time > ti))
#generate first layout using graphopt with normalized coordinates. This places the initially connected set of nodes in the middle. If you use fruchterman.reingold it will place that initial set in the outer ring.
layout.old <- norm_coords(layout.graphopt(gt), xmin = -1, xmax = 1, ymin = -1, ymax = 1)

#total time of the dynamics
total_time <- max(E(g)$time)
#This is the time interval for the animation. In this case is taken to be 1/10
#of the time (i.e. 10 snapshots) between adding two consecutive nodes
dt <- 0.1
#Output for each frame will be a png with HD size 1600x900 <img src="http://estebanmoro.org/wp-includes/images/smilies/simple-smile.png" alt=":)" class="wp-smiley" style="height: 1em; max-height: 1em;">
png(file="animExample%03d.png", width=1600,height=900)


#Time loop starts
for(time in seq(ti,total_time,dt)){
  #remove edges which are not present
  gt <- delete.edges(g,which(E(g)$time > time))
  gt <- delete.edges(gt,which(E(gt)$time <= time-dt*10))
  
  #with the new graph, we update the layout a little bit
  ptm<-proc.time()
  layout.new <- layout_with_fr(gt,coords=layout.old,niter=100,start.temp=0.05,grid="nogrid")
  #plot the new graph
  print(proc.time()-ptm)
  
  nodes_size <- rep(0,vcount(g))
  for (p in seq(1,vcount(gt))){
    nodes_size[p]<-log(((subSizes[V(gt)$name[p]==as.vector(subSizes[,1]),2])/totalComm)*10000)/50
  }
  nodesTransferSize<-nodes_size[degree(gt)>0]
  V(g)$size[degree(gt)>0] <- nodesTransferSize*50
  V(g)$size[is.na(V(g)$size)]<-(-Inf)
  gt<-simplify(gt)
  
  fgreedy<-fastgreedy.community(gt)
  community$membership

  ptm<-proc.time()
  for (q in seq(1,length(unique(community$membership)))){
    V(gt)$color[!is.na(match(as.data.frame(V(gt)$name)[,1],
          as.data.frame(community[unique(community$membership)[q]])[,1]))]<-intense.pal[q]
    
  }
  print(proc.time()-ptm)
  # plot(gt,layout=layout.new,vertex.label="",vertex.size=1+2*log(degree(gt)),vertex.frame.color=V(g)$color,edge.width=1.5,edge.curved=TRUE,asp=9/16,margin=-0.15)
  plot(gt,layout=layout.new,vertex.label=V(gt)$name,vertex.size=V(g)$size, vertex.frame.color=V(g)$color,edge.width=1.5,edge.curved=TRUE,asp=9/16,margin=-0.15)
  
    #use the new layout in the next round
  layout.old <- layout.new
}
dev.off()