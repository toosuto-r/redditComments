library(RPostgreSQL)
setwd("~/R/data")

# set the limit on the number of top words to be polled
wordLim<-3000

ptmMain<-proc.time()
#set a placeholder frame for the weight data later, as well as the sub sizes
timeLinkTmp<-data.frame(id1=double(),id2=double(),weight=double(),time=double())

##connect to db and return test results

ptm<-proc.time()
# get the DB password
pw<-getPass()

# files to read
topSubsFile<-"~/R/data/rcomments/topSubs300-dates_full.txt"
stopWordsFile<-"~/R/data/rcomments/englishStop.txt"

# invariant files to write
dbDataOut<-"~/R/data/rcomments/fullSubTableStop.txt"
subSizeTableOut<-"~/R/data/rcomments/subSizes_allDates_full.txt"


# get the list of subreddits and their words
fName<-topSubsFile
topSubTable<-read.table(fName,sep=",",header = TRUE,check.names=FALSE)

# get the modified list of stop words
fName<-stopWordsFile
stopWords<-read.table(fName,sep=",",header = FALSE)

lim<-nrow(topSubTable)
dateLim<-dim(topSubTable)[2]


# instantiate a table for holding the subreddit and their sizes for each date
subSizeTable<-as.data.frame(matrix(nrow=lim,ncol=dateLim*2))


startSet<-187
if (startSet>1){
  fName<-paste("~/R/data/rcomments/lexicon_main.txt",sep="")
  mainLexicon<-read.table(fName,sep=",",header = TRUE,check.names=FALSE)
  startSet<-dim(mainLexicon)[2]
}

# create word traces for each week without subreddit distinction - full reddit lexicon


# set a loop to run each date snapshot from the top subs for each date
for (dateSet in seq(startSet,dateLim)){
  ptm<-proc.time()
  #connect to main comment database
  drv<-dbDriver("PostgreSQL")
  con<- dbConnect(drv,host="remote.picodoc.org",port=12360,dbname="reddit",user="reddit",password=pw)
  
  
  # define the query to return the pop. table (a list of all subs)
  # ensure checking only subs which exist
  theseTopSubs<-topSubTable[,dateSet]
  noNASubList<-theseTopSubs[!is.na(theseTopSubs)]
  preSubs<-paste(noNASubList,collapse="','")
  subList<-paste("'",preSubs,"'",sep="")
  
  # send one large query to get the full result set for this date
  currQuery<-paste("SELECT * FROM word_counts WHERE subreddit IN (", subList, ") AND date='",names(topSubTable)[dateSet],"' ORDER BY subreddit, count DESC;",sep="")
  result<- dbSendQuery(con, statement = currQuery)
  dataOut <- fetch(result, -1)
  dbClearResult(dbListResults(con)[[1]])
  
  # remove stop words (including "deleted" etc.)
  dataOut<-dataOut[-which(dataOut$word %in% stopWords[,1]),]
  
  # write the pop. table for the current sub
  fName<-dbDataOut
  write.table(dataOut,fName,sep=",")
  
  # dataOut<-read.table(fName,sep=",",header=TRUE)
  
  dbDisconnect(con)
  
  cat("time elapsed for main DB call (",  names(topSubTable)[dateSet], "): ",(proc.time()-ptm)[3],"s.\n",sep="")
  
  
  ptm<-proc.time()
  
  # set the initial subreddit (most populous) table with starting word list
  # and associated counts
  currSub<-dataOut[which(dataOut$subreddit %in% topSubTable[1,dateSet]),c("word","count")][1:wordLim,]
  
  # run through all subs and merge into the current ever-larger word freq table
  # i.e. full outer join
  # NOTE to make this more efficient, merge onto the first column only and expand with rbind?
  for (p in seq(2,lim)){
    tmpSub<-currSub
    newSub<-dataOut[which(dataOut$subreddit %in% topSubTable[p,dateSet]),c("word","count")][1:wordLim,]
    currSub<-merge(tmpSub,newSub[!is.na(newSub[,2]),],by="word",all=TRUE)
  }
  
  rm(dataOut)
  
  names(currSub)<-c("wordList",as.vector(topSubTable[,dateSet]))
  # freqTable<-currSub[order(currSub[,2], decreasing=TRUE),]
  freqTable<-currSub
  rm(currSub)
  # currSub<-currSub[order(currSub[,c(as.vector(topSubTable$'2015-02-16'))], decreasing=TRUE),]
  freqTable[is.na(freqTable)]<-0
  
  
  wordSums<-rowSums(freqTable[,2:dim(freqTable)[2]])
  thisLexicon<-data.frame(freqTable[,1],wordSums)
  names(thisLexicon)<-c("word","wordSums")
  
  # run a loop and for each pairing calculate the difference in word use percentages 
  # and multiply by the sum of word use between them to favour high-ranking words
  # note zero in both does not contribute to the weight sum
  
  # additionally, record the sub sizes along with the sub
  
  fName<-paste("~/R/data/rcomments/freqTable_remote_recent_",names(topSubTable)[dateSet],".txt",sep="")
  write.table(freqTable,fName,sep=",")
  
  rm(freqTable)
  
  fName<-paste("~/R/data/rcomments/lexicon_recent_",names(topSubTable)[dateSet],".txt",sep="")
  write.table(thisLexicon,fName,sep=",")
  
  # Once we have the new lexicon, merge it onto a table of the last one
  # first time just push thisLex into mainLex
  if (dateSet>1){
    mainLexicon<-merge(mainLexicon,thisLexicon,by="word",all=TRUE)
  } else {
    mainLexicon<-thisLexicon
  }
  
  names(mainLexicon)<-c("word",names(topSubTable)[1:dateSet])
  
  cat("time elapsed for table merge (",  names(topSubTable)[dateSet], "): ",(proc.time()-ptm)[3],"s.\n",sep="")
  
  fName<-paste("~/R/data/rcomments/lexicon_main.txt",sep="")
  write.table(mainLexicon,fName,sep=",")
  
}

