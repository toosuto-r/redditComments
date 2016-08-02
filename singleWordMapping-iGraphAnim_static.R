#this version of the script has been tested on igraph 1.0.1
#load libraries
library("igraph")
library("RColorBrewer")
library("rgexf")
library("colorspace")
library("plotrix")
library("ggplot2")

ptmMain<-proc.time()

interestWord<-"hillary"
interestLabel<-"HILLARY"

dateSeq<-seq(as.Date("2007-10-15"),as.Date("2016-02-22"),"weeks")



baseDir<-"~/R/data/rcomments/"



#topSubTableFile<-"~/R/data/rcomments/topSubs300-dates_recent.txt"


#load the edges with time stamp
#there are three columns in edges: id1,id2,time
edges <- read.table("~/R/data/rcomments/subRelTable_remote_2016-02-22_thresh.txt",header=T,sep=",",check.names = FALSE)

plotOpacity<-0.05

#generate a cool palette for the graph (all light colours, so label text is legible)
intPal<-as.data.frame(rbind(c(115,170,187,0.65),
                            c(84,178,152,0.65),
                            c(145,166,168,0.65),
                            c(190,150,196,0.65),
                            c(209,153,79,0.65),
                            c(226,142,127,0.65),
                            c(217,143,167,0.65),
                            c(122,169,211,0.65),
                            c(160,170,115,0.65),
                            c(188,187,100,0.65),
                            c(114,206,238,0.65),
                            c(211,188,241,0.65),
                            c(102,220,161,0.65),
                            c(223,195,219,0.65),
                            c(89,228,224,0.65),
                            c(154,227,129,0.65),
                            c(167,221,221,0.65),
                            c(151,228,192,0.65),
                            c(221,219,95,0.65),
                            c(193,228,158,0.65)))

# set the colour for zero-size nodes, should they appear
col_blank<-c(0,0,0,0)

#set sensible colour limits on percentages - let's make sure there's good contrast nheah
colMin<-0
colMax<-0.01


# read in the sizes of all subreddits to convert to node size later
subSizes <- read.table("~/R/data/rcomments/subSizes_allDates_recent.txt",header=TRUE, sep = ",")

# set starting time as the first time in the column
#ti <- min(edges$time)

# or set it as the first point of interest
ti<-300


#total time of the dynamics
#total_time <- max(E(g)$time)

# or max time of interest (437 is the max)
total_time <- 437

#select only the relevant edges 
edges<-edges[which(edges[,4]>=ti),]

# inner join to get a table for each unique pairing, average the weight, and discard the time

avEdgeFile<-paste("~/R/data/rcomments/avEdges_",dateSeq[ti],"-",dateSeq[total_time],".txt",sep="")

uniqEdge<-unique(edges[,1:2])

if (!file.exists(avEdgeFile)==TRUE){
  avEdges<-data.frame(id1=double(),id2=double(),weight=double())
  
  for (q in seq(1,nrow(uniqEdge))){
    avEdges<-rbind(avEdges,data.frame(id1=uniqEdge[q,1],id2=uniqEdge[q,2],weight=mean(merge(edges,uniqEdge[q,1:2],by=c("id1","id2"))$weight)))
  }
} else {
  avEdges2<-read.table(avEdgeFile)
}

#generate the full graph with all nodes (not used for plotting)
g <- graph.data.frame(avEdges,directed=F)

#remove edges which are not present - note: keeps ALL nodes
gt <- g
# generate first layout using graphopt with normalized coordinates. 
# This places the initially connected set of nodes in the middle. 
# If you use fruchterman.reingold it will place that initial set in the outer ring.
layout.old <- norm_coords(layout.graphopt(gt), xmin = -1, xmax = 1, ymin = -1, ymax = 1)



#This is the time interval for the animation. Since we're moving from one solved set to the next, set as 1
dt <- 1



# make a frame to hold the node colours
nodes_col<-as.data.frame(matrix(nrow=vcount(gt),ncol=4))

# load in the top subs with counts for community drill-down
# fName<-topSubTableFile
# topSubTable<-read.table(fName,sep=",",header = TRUE,check.names=FALSE)

metricHolderNo<-c()
metricHolderFrac<-c()

# run the single gexf calculation (including colour information - needs to be loaded in initial gexf)
# outside the loop, and iterate for a long time in layout!
# pull out subSizes from the interest file

thisInterestFile<-paste("~/R/data/rcomments/subSize_interest_remote_",dateSeq[time],".txt",sep="")
interestVals<-read.table(thisInterestFile,header=TRUE,sep=",",row.names=1)
interestVals[is.na(interestVals)]<-0
subSizes<-data.frame(row.names(interestVals),interestVals[,1])
names(subSizes)<-c("subs","sizes")


# pull out the word of interest usage as well
interestCols<-data.frame(row.names(interestVals),interestVals[eval(interestWord)])
names(interestCols)<-c("subs","uses")

# test metrics for confinement
# confinementNo<-var(interestCols$uses)/(length(which(interestCols$uses!=0))+1)
confinementNo<-var(interestCols$uses)*mean(interestCols$uses)

# use 1-fractionalUse (by subreddit) - this causes the metric to get bigger the fewer subs use it
# note inf when no-one uses it
confinementFrac<-var(interestCols$uses)/(1-(length(which(interestCols$uses!=0)))/length(interestCols$uses))*mean(interestCols$uses)

metricHolderNo<-c(metricHolderNo,confinementNo)
metricHolderFrac<-c(metricHolderFrac,confinementFrac)

# filter interestCols to "saturate" the colours
interestCols$uses[interestCols$uses>colMax]<-colMax

#generate a colour gradient by adding 0 at the front and the cap at the end
colorGen<-color.scale(c(colMin,interestCols$uses,colMax),cs1=c(0,1),alpha=1,extremes=NA,na.color=NA,xrange=NULL,color.spec="rgb")
nodeCols<-data.frame(subs=interestCols$subs,nodeCol=colorGen[2:(length(colorGen)-1)])
nodeColRGBA<-as.data.frame(t(col2rgb(nodeCols$nodeCol, alpha=TRUE)))
row.names(nodeColRGBA)<-interestCols$subs
nodeColRGBA$alpha<-nodeColRGBA$alpha/(255*1.3)


# total comments in this time interval
totalComm<-sum(subSizes$sizes)

#with the new graph, we update the layout a little bit
layout.new <- layout_with_fr(gt,coords=layout.old,niter=10000,start.temp=10,grid="nogrid",dim=2)


nodes_size <- rep(0,vcount(gt))

# nodes_size<-log(log(log(subSizes[match(V(gt)$name,subSizes[,floor(time)*2-1]),floor(time)*2]/totalComm+1)+1)+1)*1
nodes_size<-log(subSizes[match(V(gt)$name,subSizes$subs),2]/totalComm+1.02)*10
# nodes_size[is.na(nodes_size)]<--Inf

# AskReddit is too goddamn big
nodes_size[is.na(nodes_size)]<-0
if (nodes_size[1]>=(nodes_size[2]*1.5)){
  nodes_size[1]<-nodes_size[2]*1.5
}


subsNodesTransferSize<-nodes_size[degree(gt)>0]
V(gt)$size[degree(gt)>0] <- subsNodesTransferSize
# V(gt)$size[is.na(V(gt)$size)]<-(-Inf)
V(gt)$size[is.na(V(gt)$size)]<- -Inf

gt<-simplify(gt)

# select the nodes with names listed in nodeColRGBA, and fill it with the corresponding colour that matches that name
nodes_col<-nodeColRGBA[match(V(gt)$name,row.names(nodeColRGBA)),]

# set each element of the frame for the zero-size nodes with the right blank colour element
nodes_col[which(is.na(nodes_col[,1])),1]<-col_blank[1]
nodes_col[which(is.na(nodes_col[,2])),2]<-col_blank[2]
nodes_col[which(is.na(nodes_col[,3])),3]<-col_blank[3]
nodes_col[which(is.na(nodes_col[,4])),4]<-col_blank[4]

nodesDF<-data.frame(ID = c(1:vcount(gt)), NAME = V(gt)$name)
edgesDF<-as.data.frame(get.edges(gt, c(1:ecount(gt))))

# this is the physical node location
nodes_co<-cbind(layout.new,(rep(0, vcount(gt))))

fileLoc<-paste("~/R/data/rComm_thresh_test_actualTime_", interestLabel, "_static_", time*10, ".gexf", sep="")

# plot(gt,layout=layout.new,vertex.label="",vertex.size=1+2*log(degree(gt)),vertex.frame.color=V(g)$color,edge.width=1.5,edge.curved=TRUE,asp=9/16,margin=-0.15)
# plot(gt,layout=layout.new,vertex.label=V(gt)$name,vertex.size=V(g)$size, vertex.frame.color=V(g)$color,edge.width=1.5,edge.curved=TRUE,asp=9/16,margin=-0.15)
write.gexf(nodes = nodesDF, edges = edgesDF, edgesWeight = E(gt)$weight, nodesVizAtt = list(color=nodes_col, position=nodes_co,size=V(gt)$size), defaultedgetype = "undirected", output = fileLoc)

#use the new layout in the next round
layout.old <- layout.new

colSizeFile<-paste("~/R/data/rComm_thresh_test_actualTime_",interestLabel ,"_static_colSize_", time*10, ".txt", sep="")
colSize<-data.frame(nodes_col,V(gt)$size)

write.table(colSize, colSizeFile, col.names=FALSE, row.names = FALSE, sep=",")

print(proc.time()-ptm)

write.table(avEdges, file=avEdgeFile)


# to get a spread metric, let's investigate the edge weighting for 
# nodes using the current word of interest compared to average


#theseShortPathNumbers<-shortest.paths(g,V(g)[na.omit(match(interestCols[which(interestCols$uses!=0),1],V(g)$name))],V(g)[na.omit(match(interestCols[which(interestCols$uses!=0),1],V(g)$name))],weights=NA)
theseShortPaths<-shortest.paths(g,V(g)[na.omit(match(interestCols[which(interestCols$uses!=0),1],V(g)$name))],V(g)[na.omit(match(interestCols[which(interestCols$uses!=0),1],V(g)$name))])
theseShortPaths<-theseShortPaths[upper.tri(theseShortPaths)]

# take out those pesky unconnected nodes, should they exist

if (length(which(!is.finite(theseShortPaths)))>0){
  filteredShortPaths<-as.data.frame(theseShortPaths[-which(!is.finite(theseShortPaths))])
} else {
  filteredShortPaths<-as.data.frame(theseShortPaths)
}
names(filteredShortPaths)<-"weight"

avHist<-hist(avEdges$weight,plot=FALSE,breaks=seq(0,ceiling(max(avEdges$weight)),length=251))
avHistFile<-paste("~/R/data/rComm_thresh_test_actualTime_",interestLabel ,"_avHist", ".txt", sep="")
write.table(data.frame(mids=avHist$mids,counts=avHist$counts), avHistFile, col.names=TRUE, row.names = FALSE, sep=",")

# remember the mids are the same for both avHist and selectHist
selectHist<-hist(avEdges$weight,plot=FALSE,breaks=seq(0,ceiling(max(avEdges$weight)),length=251))
selectHistFile<-paste("~/R/data/rComm_thresh_test_actualTime_",interestLabel ,"_selectHist_", time*10, ".txt", sep="")
write.table(data.frame(mids=avHist$breaks[-length(avHist$breaks)], avCounts=avHist$counts, selectCounts=selectHist$counts), selectHistFile, col.names=TRUE, row.names = FALSE, sep=",",quote=FALSE)


medWeight<-median(avEdges$weight)
medInterestWeight<-median(filteredShortPaths$weight)

medInterestWeightVec<-medInterestWeight

shortPathsFile<-paste("~/R/data/rComm_thresh_test_actualTime_",interestLabel ,"_shortPaths_", time*10, ".txt", sep="")
write.table(filteredShortPaths, shortPathsFile, col.names=FALSE, row.names = FALSE, sep=",")

mainPlot<-ggplot(data=avEdges, aes(weight))+xlim(0,20)+geom_histogram(bins=250, alpha=0.6)
mainPlot<-mainPlot+geom_histogram(data=filteredShortPaths, fill="red", bins=250, alpha=plotOpacity)

# Time loop starts - "time" is used to select the current set of edges and the current set of sub sizes
for(time in seq(ti+1,total_time,dt)){
  
  ptm<-proc.time()
  
  # pull out subSizes from the interest file
  thisInterestFile<-paste("~/R/data/rcomments/subSize_interest_remote_",dateSeq[time],".txt",sep="")
  interestVals<-read.table(thisInterestFile,header=TRUE,sep=",",row.names=1)
  interestVals[is.na(interestVals)]<-0
  subSizes<-data.frame(row.names(interestVals),interestVals[,1])
  names(subSizes)<-c("subs","sizes")
  
  # pull out the word of interest usage as well
  interestCols<-data.frame(row.names(interestVals),interestVals[eval(interestWord)])
  names(interestCols)<-c("subs","uses")
  
  # test metrics for confinement
  confinementNo<-var(interestCols$uses)*mean(interestCols$uses)
  
  # use 1-fractionalUse (by subreddit) - this causes the metric to get bigger the fewer subs use it
  # note inf when no-one uses it
  confinementFrac<-var(interestCols$uses)/(1-(length(which(interestCols$uses!=0)))/length(interestCols$uses))*mean(interestCols$uses)
  
  metricHolderNo<-c(metricHolderNo,confinementNo)
  metricHolderFrac<-c(metricHolderFrac,confinementFrac)
  
  # filter interestCols to "saturate" the colours
  interestCols$uses[interestCols$uses>colMax]<-colMax
  
  #generate a colour gradient by adding 0 at the front and the cap at the end
  colorGen<-color.scale(c(colMin,interestCols$uses,colMax),cs1=c(0,1),alpha=1,extremes=NA,na.color=NA,xrange=NULL,color.spec="rgb")
  nodeCols<-data.frame(subs=interestCols$subs,nodeCol=colorGen[2:(length(colorGen)-1)])
  nodeColRGBA<-as.data.frame(t(col2rgb(nodeCols$nodeCol, alpha=TRUE)))
  row.names(nodeColRGBA)<-interestCols$subs
  nodeColRGBA$alpha<-nodeColRGBA$alpha/(255*1.3)
  
  
  # total comments in this time interval
  totalComm<-sum(subSizes$sizes)
  
  #with the new graph, we update the layout a little bit
  #layout.new <- layout_with_fr(gt,coords=layout.old,niter=1400,start.temp=0.05,grid="nogrid",dim=2)
  
  
  nodes_size <- rep(0,vcount(gt))
  
  # nodes_size<-log(log(log(subSizes[match(V(gt)$name,subSizes[,floor(time)*2-1]),floor(time)*2]/totalComm+1)+1)+1)*1
  nodes_size<-log(subSizes[match(V(gt)$name,subSizes$subs),2]/totalComm+1.02)*10
  # nodes_size[is.na(nodes_size)]<--Inf
  
  # AskReddit is too goddamn big
  nodes_size[is.na(nodes_size)]<-0
  if (nodes_size[1]>=(nodes_size[2]*1.5)){
    nodes_size[1]<-nodes_size[2]*1.5
  }
  
  
  subsNodesTransferSize<-nodes_size[degree(gt)>0]
  V(gt)$size[degree(gt)>0] <- subsNodesTransferSize
  # V(gt)$size[is.na(V(gt)$size)]<-(-Inf)
  V(gt)$size[is.na(V(gt)$size)]<- -Inf
  
  gt<-simplify(gt)
  
  # select the nodes with names listed in nodeColRGBA, and fill it with the corresponding colour that matches that name
  nodes_col<-nodeColRGBA[match(V(gt)$name,row.names(nodeColRGBA)),]
  
  # set each element of the frame for the zero-size nodes with the right blank colour element
  nodes_col[which(is.na(nodes_col[,1])),1]<-col_blank[1]
  nodes_col[which(is.na(nodes_col[,2])),2]<-col_blank[2]
  nodes_col[which(is.na(nodes_col[,3])),3]<-col_blank[3]
  nodes_col[which(is.na(nodes_col[,4])),4]<-col_blank[4]
  
  #nodesDF<-data.frame(ID = c(1:vcount(gt)), NAME = V(gt)$name)
  #edgesDF<-as.data.frame(get.edges(gt, c(1:ecount(gt))))
  
  #fileLoc<-paste("~/R/data/rComm_thresh_test_actualTime_", interestLabel, "_static_", time*10, ".gexf", sep="")
  
  # plot(gt,layout=layout.new,vertex.label="",vertex.size=1+2*log(degree(gt)),vertex.frame.color=V(g)$color,edge.width=1.5,edge.curved=TRUE,asp=9/16,margin=-0.15)
  # plot(gt,layout=layout.new,vertex.label=V(gt)$name,vertex.size=V(g)$size, vertex.frame.color=V(g)$color,edge.width=1.5,edge.curved=TRUE,asp=9/16,margin=-0.15)
  # write.gexf(nodes = nodesDF, edges = edgesDF, edgesWeight = E(gt)$weight, nodesVizAtt = list(color=nodes_col, position=nodes_co,size=V(gt)$size), defaultedgetype = "undirected", output = fileLoc)
  
  # use the new layout in the next round
  # layout.old <- layout.new
  
  colSizeFile<-paste("~/R/data/rComm_thresh_test_actualTime_",interestLabel ,"_static_colSize_", time*10, ".txt", sep="")
  colSize<-data.frame(nodes_col,V(gt)$size)
  
  write.table(colSize, colSizeFile, col.names=FALSE, row.names = FALSE, sep=",")
  
  
  print(proc.time()-ptm)
  
  theseShortPaths<-shortest.paths(g,V(g)[na.omit(match(interestCols[which(interestCols$uses!=0),1],V(g)$name))],V(g)[na.omit(match(interestCols[which(interestCols$uses!=0),1],V(g)$name))])
  theseShortPaths<-theseShortPaths[upper.tri(theseShortPaths)]
  
  # take out those pesky unconnected nodes, should they exist
  if (length(which(!is.finite(theseShortPaths)))>0){
    filteredShortPaths<-as.data.frame(theseShortPaths[-which(!is.finite(theseShortPaths))])
  } else {
    filteredShortPaths<-as.data.frame(theseShortPaths)
  }
  names(filteredShortPaths)<-"weight"
  

  medInterestWeight<-median(filteredShortPaths$weight)
  
  medInterestWeightVec<-c(medInterestWeightVec,medInterestWeight)
  
  # remember the mids are the same for both avHist and selectHist
  selectHist<-hist(filteredShortPaths$weight,plot=FALSE,breaks=seq(0,ceiling(max(avEdges$weight)),length=251))
  selectHistFile<-paste("~/R/data/rComm_thresh_test_actualTime_",interestLabel ,"_selectHist_", time*10, ".txt", sep="")
  write.table(data.frame(mids=avHist$breaks[-length(avHist$breaks)], avCounts=avHist$counts, selectCounts=selectHist$counts), selectHistFile, col.names=TRUE, row.names = FALSE, sep=",",quote=FALSE)
  
  shortPathsFile<-paste("~/R/data/rComm_thresh_test_actualTime_",interestLabel ,"_shortPaths_", time*10, ".txt", sep="")
  write.table(filteredShortPaths, shortPathsFile, col.names=FALSE, row.names = FALSE, sep=",")
  
  mainPlot<-mainPlot+geom_histogram(data=filteredShortPaths, fill="red", bins=250, alpha=plotOpacity)
  
}

metricFrame<-data.frame((dateSeq[ti:total_time]),metricHolderNo)
names(metricFrame)<-c("date","distUse")

metricHolderFile<-paste("~/R/data/rComm_thresh_test_actualTime_",interestLabel ,"_metric.txt", sep="")
write.table(metricFrame, metricHolderFile, col.names=TRUE, row.names = FALSE, sep=",",quote=FALSE)

medInterestFrame<-data.frame(ti:total_time, medInterestWeightVec)
medInterestFile<-paste("~/R/data/rComm_thresh_test_actualTime_",interestLabel ,"_medInterest.txt", sep="")
write.table(medInterestFrame, medInterestFile, col.names=FALSE, row.names = FALSE, sep=",")


mainPlot

print(proc.time()-ptmMain)