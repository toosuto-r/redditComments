library(RPostgreSQL)
setwd("~/R/data")

# set the limit on the number of top words to be polled
wordLim<-3000

ptmMain<-proc.time()
#set a placeholder frame for the weight data later, as well as the sub sizes
timeLinkTmp<-data.frame(id1=double(),id2=double(),weight=double(),time=double())

##connect to db and return test results

ptm<-proc.time()


# files to read
topSubsFile<-"~/R/data/rcomments/topSubs300-dates_full.txt"
stopWordsFile<-"~/R/data/rcomments/englishStop.txt"

# invariant files to write
dbDataOut<-"~/R/data/rcomments/fullSubTableStop.txt"
subSizeTableOut<-"~/R/data/rcomments/subSizes_allDates_full.txt"


# get the list of subreddits and their words
fName<-topSubsFile
topSubTable<-read.table(fName,sep=",",header = TRUE,check.names=FALSE)
# flip it left-right, so newer columns are at higher indices
topSubTable<-topSubTable[,c(1,seq(ncol(topSubTable),2))]

# get the modified list of stop words
fName<-stopWordsFile
stopWords<-read.table(fName,sep=",",header = FALSE)

lim<-nrow(topSubTable)
dateLim<-dim(topSubTable)[2]


# instantiate a table for holding the subreddit and their sizes for each date
subSizeTable<-as.data.frame(matrix(nrow=lim,ncol=dateLim*2))


#### Here, let's check what's already in the DB - this will determine the starting dataset
# get the DB password
pw<-getPass("local")

drv<-dbDriver("PostgreSQL")
con<- dbConnect(drv,host="localhost",port=5432,dbname="fulllexicon",user="postgres",password=pw)

currQuery<-"SELECT COUNT(*) FROM information_schema.columns WHERE table_name='lexicon'"
result<- dbSendQuery(con, statement = currQuery)
dataOut <- fetch(result, -1)
dbClearResult(dbListResults(con)[[1]])
startSet<-as.numeric(dataOut)

currQuery<-"SELECT * FROM lexicon LIMIT 1"
result<- dbSendQuery(con, statement = currQuery)
dataOut <- fetch(result, -1)


##### TABLE REORDER #####
# ONLY TO BE RUN IF TABLE IS IN REVERSE ORDER
# currQuery<-paste("CREATE TABLE reorderlexicon AS SELECT ",paste("word",paste(rev(names(dataOut)[2:length(dataOut)]),collapse=", "),sep=", "), " FROM lexicon", sep="")
# result<- dbSendQuery(con, statement = currQuery)
# 
# dbSendQuery(con,statement="DROP TABLE lexicon;")
# dbSendQuery(con,statement="ALTER TABLE reorderlexicon RENAME TO lexicon;")

dbDisconnect(con)

startSet<-startSet+1

# create word traces for each week without subreddit distinction - full reddit lexicon

# set a loop to run each date snapshot from the top subs for each date
for (dateSet in seq(startSet,dateLim)){
  ptm<-proc.time()
  # get the DB password
  pw<-getPass("remote")
  
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
  names(thisLexicon)<-c("word",paste("w",gsub("-","",names(topSubTable)[dateSet]),sep=""))
  
  # run a loop and for each pairing calculate the difference in word use percentages 
  # and multiply by the sum of word use between them to favour high-ranking words
  # note zero in both does not contribute to the weight sum
  
  # additionally, record the sub sizes along with the sub
  
  fName<-paste("~/R/data/rcomments/freqTable_remote_recent_",names(topSubTable)[dateSet],".txt",sep="")
  write.table(freqTable,fName,sep=",")
  
  rm(freqTable)
  
  fName<-paste("~/R/data/rcomments/lexicon_recent_",names(topSubTable)[dateSet],".txt",sep="")
  write.table(thisLexicon,fName,sep=",")
  
  # Once we have the new lexicon, put it into a table
  pw<-getPass("local")
  drv<-dbDriver("PostgreSQL")
  con<- dbConnect(drv,host="localhost",port=5432,dbname="fulllexicon",user="postgres",password=pw)
  dbWriteTable(con,name="tmplexicon",value=thisLexicon,row.names=FALSE, overwrite=TRUE)
  
  currQuery<-paste("CREATE TABLE newlexicon AS SELECT * FROM lexicon FULL OUTER JOIN tmplexicon USING (word) WITH DATA;",sep="")
  result<- dbSendQuery(con, statement = currQuery)
  dbSendQuery(con,statement="DROP TABLE lexicon;")
  dbSendQuery(con,statement="ALTER TABLE newlexicon RENAME TO lexicon;")
  
  
  #dataOut <- fetch(result, -1)
  dbClearResult(dbListResults(con)[[1]])
  
  dbDisconnect(con)
  
  cat("time elapsed for table merge (",  names(topSubTable)[dateSet], "): ",(proc.time()-ptm)[3],"s.\n",sep="")
}

