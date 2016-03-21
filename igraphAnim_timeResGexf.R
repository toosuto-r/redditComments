#this version of the script has been tested on igraph 1.0.1
#load libraries
library("igraph")
library("RColorBrewer")
library("rgexf")
library("colorspace")

#load the edges with time stamp
#there are three columns in edges: id1,id2,time
edges <- read.table("~/R/data/rcomments/subRelTable_remote_2015-01-05_thresh.txt",header=T)

#generate the full graph
g <- graph.data.frame(edges,directed=F)

#generate a cool palette for the graph (darker colors = older nodes)

intPalHex<-c(rgb(147,23,69, maxColorValue=255),
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


intPal<-as.data.frame(rbind(c(147,23,69,0.5),
                            c(142,192,1,0.5),
                            c(107,191,195,0.5),
                            c(135,52,174,0.5),
                            c(247,146,75,0.5),
                            c(42,69,104,0.5),
                            c(97,71,12,0.5),
                            c(223,146,189,0.5),
                            c(14,101,67,0.5),
                            c(215,37,151,0.5),
                            c(86,24,91,0.5),
                            c(111,181,244,0.5),
                            c(120,36,8,0.5),
                            c(246,93,136,0.5),
                            c(245,133,119,0.5),
                            c(14,128,47,0.5),
                            c(83,56,72,0.5),
                            c(156,113,17, 0.5),
                            c(38,158,106,0.5),
                            c(169,83,18,0.5)))

col_blank<-c(128,128,128,0.5)

intense.pal <- intPalHex
#colors for the nodes are chosen from the very beginning

subSizes <- read.table("C:/Users/Ryan/Documents/R/data/rcomments/subSizes_allDates.txt",header=TRUE, sep = ",")

# for (p in seq(1,vcount(g))){
#   nodes_size[p]<-log(((subSizes[V(g)$name[p]==as.vector(subSizes[,1]),2])/totalComm)*10000)/50
# }

# nodes_size <- 1

#time in the edges goes from 1 to 300. We kick off at time 3
ti <- min(edges$time)
#remove edges which are not present - note: keeps ALL nodes
gt <- delete.edges(g,which(E(g)$time < ti))
#generate first layout using graphopt with normalized coordinates. This places the initially connected set of nodes in the middle. If you use fruchterman.reingold it will place that initial set in the outer ring.
layout.old <- norm_coords(layout.graphopt(gt), xmin = -1, xmax = 1, ymin = -1, ymax = 1)

#total time of the dynamics
total_time <- max(E(g)$time)
#This is the time interval for the animation. In this case is taken to be 1/10
#of the time (i.e. 10 snapshots) between adding two consecutive nodes
dt <- 0.25
#Output for each frame will be a png with HD size 1600x900 <img src="http://estebanmoro.org/wp-includes/images/smilies/simple-smile.png" alt=":)" class="wp-smiley" style="height: 1em; max-height: 1em;">
# png(file="animExample%03d.png", width=1600,height=900)

nodes_col<-as.data.frame(matrix(nrow=vcount(gt),ncol=4))
nodes_col_hex<-as.data.frame(matrix(nrow=vcount(gt),ncol=4))

#Time loop starts - "time" is used to select the current set of edges and the current set of sub sizes
for(time in seq(ti,total_time,dt)){
  #remove edges which are not present
  gt <- delete.edges(g,which(E(g)$time > time))
  gt <- delete.edges(gt,which(E(g)$time <= time-1))
  
  totalComm<-sum(subSizes[,2*floor(time)])
  
  #with the new graph, we update the layout a little bit
  
  
  #### SOMETHING WITH SIZES - EDGES GOING TO ZERO SIZE NODES?
  
  
  ptm<-proc.time()
  layout.new <- layout_with_fr(gt,coords=layout.old,niter=500,start.temp=0.05,grid="nogrid",dim=2)
  #plot the new graph
  print(proc.time()-ptm)
  revTime<-total_time+1-time
  nodes_size <- rep(0,vcount(gt))
#   for (p in seq(1,vcount(gt))){
#     nodes_size[p]<-log(((subSizes[V(gt)$name[p]==as.vector(subSizes[,floor(time)*2-1]),floor(time)*2])/totalComm)*10000)/500
#   print(p)
#   }
#   
  # nodes_size<-log(log(log(subSizes[match(V(gt)$name,subSizes[,floor(time)*2-1]),floor(time)*2]/totalComm+1)+1)+1)*1
  nodes_size<-log(subSizes[match(V(gt)$name,subSizes[,floor(time)*2-1]),floor(time)*2]/totalComm+1.02)*1
  # nodes_size[is.na(nodes_size)]<--Inf
  nodes_size[is.na(nodes_size)]<-0
  nodes_size[1]<-nodes_size[2]*1.5
  
  
  subsNodesTransferSize<-nodes_size[degree(gt)>0]
  V(gt)$size[degree(gt)>0] <- subsNodesTransferSize
  # V(gt)$size[is.na(V(gt)$size)]<-(-Inf)
  V(gt)$size[is.na(V(gt)$size)]<-(0)
  
  gt<-simplify(gt)
  
  community<-fastgreedy.community(gt)
  
  ptm<-proc.time()
  for (q in seq(1,length(unique(community$membership)))){
    nodes_col[!is.na(match(as.data.frame(V(gt)$name)[,1],
                           as.data.frame(community[unique(community$membership)[q]])[,1])),]<-intPal[q,]
    nodes_col_hex[!is.na(match(as.data.frame(V(gt)$name)[,1],
                               as.data.frame(community[unique(community$membership)[q]])[,1])),]<-intense.pal[q]    
  }
  
  nodes_col[which(is.na(nodes_col[,1])),1]<-col_blank[1]
  nodes_col[which(is.na(nodes_col[,2])),2]<-col_blank[2]
  nodes_col[which(is.na(nodes_col[,3])),3]<-col_blank[3]
  nodes_col[which(is.na(nodes_col[,4])),4]<-col_blank[4]
  
  print(proc.time()-ptm)
  
  nodesDF<-data.frame(ID = c(1:vcount(gt)), NAME = V(gt)$name)
  edgesDF<-as.data.frame(get.edges(gt, c(1:ecount(gt))))
  
  fileLoc<-paste("~/R/data/rComm_thresh_test_actualTime_sml14_", time*10, ".gexf", sep="")
  
  nodes_co<-cbind(layout.new,(rep(0, vcount(gt))))
  
  # plot(gt,layout=layout.new,vertex.label="",vertex.size=1+2*log(degree(gt)),vertex.frame.color=V(g)$color,edge.width=1.5,edge.curved=TRUE,asp=9/16,margin=-0.15)
  # plot(gt,layout=layout.new,vertex.label=V(gt)$name,vertex.size=V(g)$size, vertex.frame.color=V(g)$color,edge.width=1.5,edge.curved=TRUE,asp=9/16,margin=-0.15)
  write.gexf(nodes = nodesDF, edges = edgesDF, edgesWeight = E(gt)$weight, nodesVizAtt = list(color=nodes_col, position=nodes_co,size=V(gt)$size), defaultedgetype = "undirected", output = fileLoc)
  
  #use the new layout in the next round
  layout.old <- layout.new
}
# dev.off()