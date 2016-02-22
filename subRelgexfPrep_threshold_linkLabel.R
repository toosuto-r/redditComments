relResults<-read.table("C:/Users/Ryan/Documents/R/data/rcomments/subRelation.txt", header=TRUE, sep=",")
maxLinks<-read.table("C:/Users/Ryan/Documents/R/data/rcomments/maxLinks.txt", header=TRUE, sep=",")
minLinks<-read.table("C:/Users/Ryan/Documents/R/data/rcomments/minLinks.txt", header=TRUE, sep=",")



matRes<-as.matrix(relResults)
# thresh<-mean(matRes,na.rm=TRUE)

#set the threshold at some reasonable appropriate value
# thresh<-as.double(quantile(matRes, na.rm=TRUE)[2]*0.5)
thresh<-as.double(quantile(matRes, 0.05, na.rm=TRUE))


#two way table taken in - select the row name and all non-NA
#column names, and put the weight as the relation
geOut<-data.frame(sub1=double(),sub2=double(),rel=double())
nodeLabel<-data.frame(catLabel=double())


count<-0
discard<-0

subNames<-row.names(relResults)
subLim<-length(subNames)

for (p in seq(1,subLim-1)){
  
  for (q in seq(p+1,subLim)){
    
    if (relResults[p,q]<=thresh){
      count<-count+1
      geOut[count,1]<-subNames[p]
      geOut[count,2]<-subNames[q]
      # use percentage of match - lower is better
      geOut[count,3]<-(1-relResults[p,q]/thresh)*100
    } else {
      count<-count
      discard<-discard+1
    }
    nodeLabel[count]<-paste(maxLabel[count,3],minLabel[count,3],sep=",")
  }
}



fName<-"~/R/data/rcomments/subRelationPreGexf_avThresh.txt"
write.table(geOut,fName,quote=FALSE,sep="\t",col.names=FALSE, row.names=FALSE)

fName2<-"~/R/data/rcomments/subRelationPreGexf_avThresh_edgeLabels.txt"
write.table(nodeLabel,fName2,quote=FALSE,sep="\t",col.names=FALSE, row.names=FALSE)


