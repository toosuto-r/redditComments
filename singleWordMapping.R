#comes in two parts - 
# one is to build a full "map" of all top 300 subreddits and their connectivity
#the frequency tables are already generated, so they just need to be imported and compared

ptm<-proc.time()

# get the dates out
dateSeq<-seq(as.Date("2007-10-15"),as.Date("2016-02-22"),"weeks")
filePrefix<-"~/R/data/rcomments/freqTable_remote_recent_"

#define a holder for the time-resolved relational data
timeLinkTmp<-data.frame(id1=double(),id2=double(),weight=double(),time=double())


# define a list of words of interest, for which we'll selectively take 
# out the percentage use and assign it to a subSize/wordColour table
interestList<-c("trump",
                "hillary",
                "sanders",
                "doge",
                "kanye",
                "automatically",
                "bottles",
                "beer",
                "apple",
                "concerns",
                "bro",
                "bae",
                "fam",
                "u",
                "xd",
                "bacon")

for (p in 1:length(dateSeq)){
  thisFile<-paste(filePrefix,dateSeq[p],".txt",sep="")
  #print(thisFile)
  
  
  # get in the word table (takes a few s - still faster than a DB call)
  thisTable<-read.table(thisFile,header=TRUE,sep=",")
  
  lim<-ncol(thisTable)-1
  wordSums<-colSums(thisTable[,1:lim+1])
  
  #check to see the real, useful values of the table - might be padding up to 300 in early weeks
#   if (!all(!names(thisTable)=="NA.")){
#     print(22)
#     thisTable<-thisTable[,1:which(names(thisTable)=="NA.")-1]
#   }

  if (!all(!wordSums==0)){
    thisTable<-thisTable[,-((which(wordSums==0)+1))]
  }
  
  print(dateSeq[p])
  
  lim<-ncol(thisTable)-1
  wordSums<-colSums(thisTable[,1:lim+1])
  
  # lim is the no. of subreddits looked at (generally 300)
  
  # get the table as percentages (not necessary if done during the subRelTable formation)
  # thisTable<-sweep(thisTable[,2:lim+1],2,colSums(thisTable[,2:lim+1]),"/")
  # thisTable<-thisTable*100
  
  # generate a list of subreddit sizes and prevalance of certain words
  subSizes<-data.frame(subs=names(thisTable)[2:length(names(thisTable))],wordSums)
  
  interestTrace<-sweep(thisTable[match(interestList,thisTable[,1]),1:lim+1],2,colSums(thisTable[,1:lim+1]),"/")
  interestTrace<-interestTrace*100
  #interestTrace<-cbind(thisTable[match(interestList,thisTable[,1]),1],interestTrace)
  #names(interestTrace)[1]<-"word"
  row.names(interestTrace)<-interestList
  
  theseInterestSizes<-cbind(subSizes$wordSums,t(interestTrace))
  names(theseInterestSizes)[1]<-"wordSum"
  
  fName<-paste("~/R/data/rcomments/subSize_interest_remote_",dateSeq[p],".txt",sep="")
  write.table(theseInterestSizes,fName,sep=",")
  
  # make the sub relation table again
  subRelTable<-as.data.frame(matrix(nrow=lim,ncol=lim))
  
  for(s in seq(1,lim-1)){
    currPer<-thisTable[,s+1]/sum(thisTable[,s+1])*100
    for(t in seq(s+1,lim)){
      scanPer<-thisTable[,t+1]/sum(thisTable[,t+1])*100
      weight<-sum((abs(currPer-scanPer)+0)*(currPer+scanPer))
      subRelTable[s,t]<-weight
    }
  }
  
  names(subRelTable)<-names(thisTable)[1:lim+1]
  row.names(subRelTable)<-names(subRelTable)
  
  #round the subRelTable to make it a little nicer in display
  subRelRound<-subRelTable
  for (v in seq(1,dim(subRelTable)[2])){
    subRelRound[,v]<-signif(subRelTable[,v],3)
  }
  
  fName<-paste("~/R/data/rcomments/subRelTable_remote_",dateSeq[p],".txt",sep="")
  write.table(subRelRound,fName,sep=",")
  
  
  # use the log of the table - absolute values are too lacking in contrast
  matRes<-as.matrix(subRelTable)
  matRes<-log(matRes)
  
  #set the threshold at some reasonable appropriate value
  thresh<-as.double(quantile(matRes, 0.05, na.rm=TRUE))
  
  
  #two way table taken in - select the row name and all non-NA
  #column names, and put the weight as the relation
  geOut<-data.frame(id1=double(),id2=double(),weight=double())
  
  count<-0
  discard<-0
  fullCount<-0
  
  subNames<-names(subRelTable)
  subLim<-length(subNames)
  
  #
  for (matP in seq(1,lim-1)){
    for (q in seq(matP+1,lim)){
      if (matRes[matP,q]<=thresh){
        count<-count+1
        fullCount<-fullCount+1
        
        geOut[count,1]<-subNames[matP]
        geOut[count,2]<-subNames[q]
        # use percentage of match - lower is better
        geOut[count,3]<-(1-matRes[matP,q]/thresh)*100
      } else {
        count<-count
        discard<-discard+1
      }
    }
  }
  
  # add the time (in this case the date index) column to the edge list
  thisTime<-as.data.frame(rep(p,dim(geOut)[1]))
  names(thisTime)<-"time"
  geTime<-cbind(geOut,thisTime)
  
  # then vertically cat to get a time-dynamic list
  timeLinkTmp<-rbind(timeLinkTmp,geTime)
  
  fName<-paste("~/R/data/rcomments/subRelTable_remote_",dateSeq[p],"_thresh.txt",sep="")
  write.table(timeLinkTmp,fName,sep=",")
  
  
}

print(proc.time()-ptm)

# two is the (relatively simpler) coloration of this map to represent prevalance of a certain word

