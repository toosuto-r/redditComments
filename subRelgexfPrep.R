relResults<-read.table("F:/Data/rcomments/subRelation.txt", header=TRUE, sep=",")

#two way table taken in - select the row name and all non-NA
#column names, and put the weight as the relation
geOut<-data.frame(sub1=double(),sub2=double(),rel=double())

count<-0

subNames<-row.names(relResults)
subLim<-length(subNames)

    for (p in seq(1,subLim-1)){
      
      for (q in seq(p+1,subLim)){
        
        count<-count+1
        
        geOut[count,1]<-subNames[p]
        geOut[count,2]<-subNames[q]
        geOut[count,3]<-relResults[p,q]
        
  }
}


fName<-"F:/Data/rcomments/subRelationPreGexf.txt"
write.table(geOut,fName,quote=FALSE,sep="\t",col.names=FALSE, row.names=FALSE)