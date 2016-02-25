relResults<-read.table("C:/Users/Ryan/Documents/R/data/rcomments/subRelation-re3.txt", header=TRUE, sep=",")

matRes<-as.matrix(relResults)
matRes<-log(matRes)
# thresh<-mean(matRes,na.rm=TRUE)

#set the threshold at some reasonable appropriate value
# thresh<-as.double(quantile(matRes, na.rm=TRUE)[2]*0.5)
thresh<-as.double(quantile(matRes, 0.04, na.rm=TRUE))


#two way table taken in - select the row name and all non-NA
#column names, and put the weight as the relation
geOut<-data.frame(sub1=double(),sub2=double(),rel=double())

count<-0
discard<-0

subNames<-row.names(relResults)
subLim<-length(subNames)

for (p in seq(1,subLim-1)){
  
  for (q in seq(p+1,subLim)){
    
    if (matRes[p,q]<=thresh){
      count<-count+1
      geOut[count,1]<-subNames[p]
      geOut[count,2]<-subNames[q]
      # use percentage of match - lower is better
      geOut[count,3]<-(1-matRes[p,q]/thresh)*100
    } else {
      count<-count
      discard<-discard+1
    }
  }
}



fName<-"~/R/data/rcomments/subRelationPreGexf_avThresh-re.txt"
write.table(geOut,fName,quote=FALSE,sep="\t",col.names=FALSE, row.names=FALSE)