#this version of the script has been tested on igraph 1.0.1
#load libraries
library("igraph")
library("RColorBrewer")
library("rgexf")
library("colorspace")
library("plotrix")

interestWord<-"trump"

dateSeq<-seq(as.Date("2007-10-15"),as.Date("2016-02-22"),"weeks")


baseDir<-"~/R/data/rcomments/"
#topSubTableFile<-"~/R/data/rcomments/topSubs300-dates_recent.txt"


#load the edges with time stamp
#there are three columns in edges: id1,id2,time
edges <- read.table("~/R/data/rcomments/subRelTable_remote_2016-02-22_thresh.txt",header=T,sep=",")

#generate the full graph with all nodes (not used for plotting)
g <- graph.data.frame(edges,directed=F)

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
col_blank<-c(128,128,128,0)

#set sensible colour limits on percentages - let's make sure there's good contrast nheah
colMin<-0
colMax<-0.001


# read in the sizes of all subreddits to convert to node size later
subSizes <- read.table("~/R/data/rcomments/subSizes_allDates_recent.txt",header=TRUE, sep = ",")

# set starting time as the first time in the column
ti <- min(edges$time)

# or set it as the first point of interest
ti<-390

#remove edges which are not present - note: keeps ALL nodes
gt <- delete.edges(g,which(E(g)$time < ti))
# generate first layout using graphopt with normalized coordinates. 
# This places the initially connected set of nodes in the middle. 
# If you use fruchterman.reingold it will place that initial set in the outer ring.
layout.old <- norm_coords(layout.graphopt(gt), xmin = -1, xmax = 1, ymin = -1, ymax = 1)

#total time of the dynamics
total_time <- max(E(g)$time)

# or max time of interest (437 is the max)
total_time <- 437

#This is the time interval for the animation. Since we're moving from one solved set to the next, set as 1
dt <- 1

# make a frame to hold the node colours
nodes_col<-as.data.frame(matrix(nrow=vcount(gt),ncol=4))

# load in the top subs with counts for community drill-down
# fName<-topSubTableFile
# topSubTable<-read.table(fName,sep=",",header = TRUE,check.names=FALSE)


#Time loop starts - "time" is used to select the current set of edges and the current set of sub sizes
for(time in seq(ti,total_time,dt)){
  
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
  
  # filter interestCols to "saturate" the colours
  interestCols$uses[interestCols$uses>colMax]<-colMax
  
  #generate a colour gradient by adding 0 at the front and the cap at the end
  colorGen<-color.scale(c(colMin,interestCols$uses,colMax),cs1=c(0,1),alpha=1,extremes=NA,na.color=NA,xrange=NULL,color.spec="rgb")
  nodeCols<-data.frame(subs=interestCols$subs,nodeCol=colorGen[2:(length(colorGen)-1)])
  nodeColRGBA<-as.data.frame(t(col2rgb(nodeCols$nodeCol, alpha=TRUE)))
  row.names(nodeColRGBA)<-interestCols$subs
  nodeColRGBA$alpha<-nodeColRGBA$alpha/(255*1.3)
  
  #remove edges which are not present
  gt <- delete.edges(g,which(E(g)$time > time))
  gt <- delete.edges(gt,which(E(g)$time <= time-1))
  
  # total comments in this time interval
  totalComm<-sum(subSizes$sizes)
  
  # get the full word table for this time
  #fName<-paste(baseDir,"freqTable_remote_recent_",names(topSubTable)[time],".txt",sep="")
  #freqTable<-read.table(fName,sep=",",check.names=FALSE)
  
  
  #get the current lexicon for community analysis
  #   fName<-paste(baseDir,"lexicon_recent_",names(topSubTable)[time],".txt",sep="")
  #   thisLexicon<-read.table(fName,sep=",")
  #   thisLexicon[,2]<-thisLexicon[,2]/sum(thisLexicon[,2])*100
  
  
  
  #with the new graph, we update the layout a little bit
   layout.new <- layout_with_fr(gt,coords=layout.old,niter=1400,start.temp=0.05,grid="nogrid",dim=2)

  # alernately, let's consider a more iterative approach to the solution while maintaining
  # some nodes in a central position
  
  # set AskReddit, the biggest subreddit, in the middle, and record the position of nba
#   layout.old[(V(gt)$name=="AskReddit"),]<-c(0,0)
#   layout.new <- layout_with_fr(gt,coords=layout.old,niter=200,start.temp=0.05,grid="nogrid",dim=2)
#   NBAholder<-layout.old[(V(gt)$name=="nba"),]
#   newsholder<-layout.old[(V(gt)$name=="news"),]
  
  
  # loop some no of times, and each time before re-solving, set AskReddit and NBA back in the same place
  # this allows some slow drift, but some fixing of positions as well
#   for(layoutLoop in seq(1,3)){
#     layout.old[(V(gt)$name=="AskReddit"),]<-c(0,0)
#     layout.old[(V(gt)$name=="nba"),]<-NBAholder
#     layout.old[(V(gt)$name=="news"),]<-newsholder
#     layout.new <- layout_with_fr(gt,coords=layout.old,niter=100,start.temp=0.05,grid="nogrid",dim=2)
#     layout.old<-layout.new
#   }
#   
  #plot the new graph
  print(proc.time()-ptm)
  
  
  revTime<-total_time+1-time
  
  ptm<-proc.time()
  nodes_size <- rep(0,vcount(gt))
  
  #   for (p in seq(1,vcount(gt))){
  #     nodes_size[p]<-log(((subSizes[V(gt)$name[p]==as.vector(subSizes[,floor(time)*2-1]),floor(time)*2])/totalComm)*10000)/500
  #   print(p)
  #   }
  #   
  # nodes_size<-log(log(log(subSizes[match(V(gt)$name,subSizes[,floor(time)*2-1]),floor(time)*2]/totalComm+1)+1)+1)*1
  nodes_size<-log(subSizes[match(V(gt)$name,subSizes$subs),2]/totalComm+1.02)*10
  # nodes_size[is.na(nodes_size)]<--Inf
  
  # AskReddit is too goddamn big
  nodes_size[is.na(nodes_size)]<-0
  if (nodes_size[1]>=(nodes_size[2]*1.5)){
    nodes_size[1]<-nodes_size[2]*1.5
  }
  
  #nodes_col
  
  subsNodesTransferSize<-nodes_size[degree(gt)>0]
  V(gt)$size[degree(gt)>0] <- subsNodesTransferSize
  # V(gt)$size[is.na(V(gt)$size)]<-(-Inf)
  V(gt)$size[is.na(V(gt)$size)]<- -Inf
  
  gt<-simplify(gt)
  
  #community<-fastgreedy.community(gt)
  #noCommunities<-length(unique(community$membership))
  #communityList<-sort(unique(community$membership))
  
  # note definining only 2 columns - the minimum, as columns are appended sequentially
  #communityWords<-as.data.frame(matrix(nrow=10,ncol=2))
  
  #thisCommunitySubListHolder<-vector()
  #thisCommunityIndexHolder<-vector()
  
  # this var will be the source of over- or under- represented words
  #communityRep<-as.data.frame(matrix(nrow=dim(freqTable[1]),ncol=2))
  #communityRep[,1]<-freqTable[,1]
  
  
  
  # go through all the communities, assign them colour and get the over-representation of words from that community
  #realCommunityCount<-0
  
  #   for (q in seq(1,noCommunities)){
  #     nodes_col[!is.na(match(as.data.frame(V(gt)$name)[,1],
  #                            as.data.frame(community[communityList[q]])[,1])),]<-intPal[q,]
  #     
  #     # for communities with more than a single subreddit (important for animations) record the community and the words it uses
  #     if (length(unlist(community[q]))>1){
  #       realCommunityCount<-realCommunityCount+1
  #       thisCommunityPers<-rowSums(freqTable[unlist(community[communityList[q]])])/sum(freqTable[unlist(community[communityList[q]])])*100
  #       # convert this community's percentage word use into an over- or under-representation compared to the main lexicon,
  #       # selecting more popular words
  #       communityRep[,2]<-abs(thisCommunityPers-thisLexicon[,2])*(thisCommunityPers+thisLexicon[,2])
  #       thisCommunityWords<-head(communityRep[order(-communityRep[,2]),],10)
  #       communityWords[,realCommunityCount*2-1]<-thisCommunityWords[,1]
  #       communityWords[,realCommunityCount*2]<-round(thisCommunityWords[,2],digits=3)
  # 
  #       # make a couple of long vectors with the subreddit name and index of the community to allow lookup of each sub later
  #       thisCommunitySubListHolder<-c(thisCommunitySubListHolder,as.vector(unlist(community[q])))
  #       thisCommunityIndexHolder<-c(thisCommunityIndexHolder,rep(q,length(unlist(community[q]))))
  #       
  #     }
  # 
  #   }
  #   
  #   commTitles<-rep(c("Words Used","Representation"),realCommunityCount)
  #   names(communityWords)<-commTitles
  #   
  #   thisCommunitySubList<-data.frame(thisCommunitySubListHolder,thisCommunityIndexHolder)
  #   thisCommunitySubList<-thisCommunitySubList[order(thisCommunitySubList[,1]),]
  #   fName<-paste(baseDir,"communityWords",names(topSubTable)[time],".txt",sep="")
  #   write.table(communityWords,fName,row.names=FALSE,sep=",")
  #   
  #   fName<-paste("~/R/data/rcomments/communitySubList",names(topSubTable)[time],".txt",sep="")
  #   write.table(thisCommunitySubList,fName,row.names=FALSE,sep=",")
  
  
  # select the nodes with names listed in nodeColRGBA, and fill it with the corresponding colour that matches that name
  nodes_col<-nodeColRGBA[match(V(gt)$name,row.names(nodeColRGBA)),]
  
  # set each element of the frame for the zero-size nodes with the right blank colour element
  nodes_col[which(is.na(nodes_col[,1])),1]<-col_blank[1]
  nodes_col[which(is.na(nodes_col[,2])),2]<-col_blank[2]
  nodes_col[which(is.na(nodes_col[,3])),3]<-col_blank[3]
  nodes_col[which(is.na(nodes_col[,4])),4]<-col_blank[4]
  
  nodesDF<-data.frame(ID = c(1:vcount(gt)), NAME = V(gt)$name)
  edgesDF<-as.data.frame(get.edges(gt, c(1:ecount(gt))))
  
  fileLoc<-paste("~/R/data/rComm_thresh_test_actualTime_TRUMP_", time*10, ".gexf", sep="")
  
  nodes_co<-cbind(layout.new,(rep(0, vcount(gt))))
  
  # plot(gt,layout=layout.new,vertex.label="",vertex.size=1+2*log(degree(gt)),vertex.frame.color=V(g)$color,edge.width=1.5,edge.curved=TRUE,asp=9/16,margin=-0.15)
  # plot(gt,layout=layout.new,vertex.label=V(gt)$name,vertex.size=V(g)$size, vertex.frame.color=V(g)$color,edge.width=1.5,edge.curved=TRUE,asp=9/16,margin=-0.15)
  write.gexf(nodes = nodesDF, edges = edgesDF, edgesWeight = E(gt)$weight, nodesVizAtt = list(color=nodes_col, position=nodes_co,size=V(gt)$size), defaultedgetype = "undirected", output = fileLoc)
  
  #use the new layout in the next round
  layout.old <- layout.new
  
  print(proc.time()-ptm)
}