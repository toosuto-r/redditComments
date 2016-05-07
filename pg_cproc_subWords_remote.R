library(RPostgreSQL)
setwd("~/R/data")


ptmMain<-proc.time()
#set a placeholder frame for the weight data later, as well as the sub sizes
timeLinkTmp<-data.frame(id1=double(),id2=double(),weight=double(),time=double())



dateSet<-1
##connect to db and return test results

ptm<-proc.time()
#get pass
pw<-getPass()

#get the list of subreddits and their words
fName<-"C:/Users/Ryan/Documents/R/data/rcomments/topSubs300-dates_recent.txt"
topSubTable<-read.table(fName,sep=",",header = TRUE,check.names=FALSE)


fName<-"C:/Users/Ryan/Documents/R/data/rcomments/englishStop.txt"
stopWords<-read.table(fName,sep=",",header = FALSE)

lim<-nrow(topSubTable)
dateLim<-dim(topSubTable)[2]

wordLim<-3000

subSizeTable<-as.data.frame(matrix(nrow=lim,ncol=dateLim*2))

#set a loop to run each date snapshot from the top subs for each date
for (dateSet in seq(1,dateLim)){

    #connect to main comment database
  drv<-dbDriver("PostgreSQL")
  con<- dbConnect(drv,host="remote.picodoc.org",port=12360,dbname="reddit",user="reddit",password=pw)
  
  
  #define the query to return the pop. table (a list of all subs)
  theseTopSubs<-topSubTable[,dateSet]
  noNASubList<-theseTopSubs[!is.na(theseTopSubs)]
  preSubs<-paste(noNASubList,collapse="','")
  subList<-paste("'",preSubs,"'",sep="")
  
  # send one large query to get the full
  currQuery<-paste("SELECT * FROM word_counts_recent WHERE subreddit IN (", subList, ") AND date='",names(topSubTable)[dateSet],"' ORDER BY subreddit, count DESC;",sep="")
  result<- dbSendQuery(con, statement = currQuery)
  dataOut <- fetch(result, -1)
  dbClearResult(dbListResults(con)[[1]])
  
  #remove stop words (including "deleted" etc.)
  dataOut<-dataOut[-which(dataOut$word %in% stopWords[,1]),]
  
  #write the pop. table for the current sub
  fName<-paste("C:/Users/Ryan/Documents/R/data/rcomments/fullSubTableStop.txt",sep="")
  write.table(dataOut,fName,sep=",")
  
  dataOut<-read.table(fName,sep=",",header=TRUE)
  
  dbDisconnect(con)
  
  cat("time elapsed for main DB call (",  names(topSubTable)[dateSet], "): ",(proc.time()-ptm)[3],"s.\n",sep="")
  
  
  ptm<-proc.time()
  
  # set the initial subreddit (most populous) table with starting word list
  # and associated counts
  currSub<-dataOut[which(dataOut$subreddit %in% topSubTable[1,dateSet]),c("word","count")][1:wordLim,]
  
  # run through all subs and merge into the current ever-larger word freq table
  # i.e. full outer join
  # to make this more efficient, merge onto the first column only?
  for (p in seq(2,lim)){
    tmpSub<-currSub
    newSub<-dataOut[which(dataOut$subreddit %in% topSubTable[p,dateSet]),c("word","count")][1:wordLim,]
    currSub<-merge(tmpSub,newSub[!is.na(newSub[,2]),],by="word",all=TRUE)
  }
  names(currSub)<-c("wordList",as.vector(topSubTable[,dateSet]))
  # freqTable<-currSub[order(currSub[,2], decreasing=TRUE),]
  freqTable<-currSub
  # currSub<-currSub[order(currSub[,c(as.vector(topSubTable$'2015-02-16'))], decreasing=TRUE),]
  freqTable[is.na(freqTable)]<-0
  
  
  wordSums<-rowSums(freqTable[,2:dim(freqTable)[2]])
  thisLexicon<-data.frame(freqTable[,1],wordSums)
  # run a loop and for each pairing calculate the difference in word use percentages 
  # and multiply by the sum of word use between them to favour high-ranking words
  # note zero in both does not contribute to the weight sum
  
  # additionally, record the sub sizes along with the sub
  
  fName<-paste("C:/Users/Ryan/Documents/R/data/rcomments/freqTable_remote_recent_",names(topSubTable)[dateSet],".txt",sep="")
  write.table(freqTable,fName,sep=",")
  
  fName<-paste("C:/Users/Ryan/Documents/R/data/rcomments/lexicon_recent_",names(topSubTable)[dateSet],".txt",sep="")
  write.table(thisLexicon,fName,sep=",")
  
  cat("time elapsed for table merge (",  names(topSubTable)[dateSet], "): ",(proc.time()-ptm)[3],"s.\n",sep="")
  
  subRelTable<-as.data.frame(matrix(nrow=lim,ncol=lim))
  subSizeTable[,dateSet*2-1]<-topSubTable[,dateSet]
  
  
  for(s in seq(1,lim-1)){
    subSizeTable[s,dateSet*2]<-sum(freqTable[,s+1])
    currPer<-freqTable[,s+1]/sum(freqTable[,s+1])*100
    for(t in seq(s+1,lim)){
      scanPer<-freqTable[,t+1]/sum(freqTable[,t+1])*100
      weight<-sum((abs(currPer-scanPer)+0)*(currPer+scanPer))
      subRelTable[s,t]<-weight
    }
  }
  
  # add the final subreddit size, which wasn't addressed by s
  subSizeTable[lim,dateSet*2]<-sum(freqTable[,lim+1])
  
  
  # add the row and column names from the current date
  names(subRelTable)<-as.vector(topSubTable[,dateSet])
  row.names(subRelTable)<-as.vector(topSubTable[,dateSet])
  
  #round the subRelTable to make it a little nicer in display
  subRelRound<-subRelTable
  for (v in seq(1,dim(subRelTable)[2])){
    subRelRound[,v]<-signif(subRelTable[,v],3)
  }
  
  
  fName<-paste("C:/Users/Ryan/Documents/R/data/rcomments/subRelTable_remote_recent_",names(topSubTable)[dateSet],".txt",sep="")
  write.table(subRelRound,fName,sep=",")
  
  
  cat("time elapsed subRelTable formation (",  names(topSubTable)[dateSet], "): ",(proc.time()-ptm)[3],"s.\n",sep="")
  
  ######################
  ######################
  
  # return a thresholded set of edges with associated IDs and times
  
  matRes<-as.matrix(subRelTable)
  matRes<-log(matRes)
  
  #set the threshold at some reasonable appropriate value
  # thresh<-as.double(quantile(matRes, na.rm=TRUE)[2]*0.5)
  thresh<-as.double(quantile(matRes, 0.05, na.rm=TRUE))
  
  
  #two way table taken in - select the row name and all non-NA
  #column names, and put the weight as the relation
  geOut<-data.frame(id1=double(),id2=double(),weight=double())
  
  count<-0
  discard<-0
  fullCount<-0
  
  subNames<-row.names(subRelTable)
  subLim<-length(subNames)
  
  for (p in seq(1,subLim-1)){
    
    for (q in seq(p+1,subLim)){
      
      if (matRes[p,q]<=thresh){
        
        count<-count+1
        fullCount<-fullCount+1
        
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
  
  # add the time (in this case the date index) column to the edge list
  thisTime<-as.data.frame(rep(dateSet,dim(geOut)[1]))
  names(thisTime)<-"time"
  geTime<-cbind(geOut,thisTime)
  
  # then vertically cat to get a time-dynamic list
  timeLinkTmp<-rbind(timeLinkTmp,geTime)
  
  fName<-paste("C:/Users/Ryan/Documents/R/data/rcomments/subRelTable_remote_recent_",names(topSubTable)[dateSet],"_thresh.txt",sep="")
  write.table(timeLinkTmp,fName,sep=",")
  
}

pw<-"disregard this"

fName<-paste("C:/Users/Ryan/Documents/R/data/rcomments/subSizes_allDates_recent.txt",sep="")
write.table(subSizeTable,fName,sep=",")

cat("time elapsed, full process: ",(proc.time()-ptmMain)[3],"s.\n",sep="")


