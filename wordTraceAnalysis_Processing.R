library(RPostgreSQL)
ptm<- proc.time()

pw<-getPass("local")
drv<-dbDriver("PostgreSQL")
con<- dbConnect(drv,host="localhost",port=5432,dbname="fulllexicon",user="postgres",password=pw)

# pull off the list of words in the lexicon
currQuery<-paste("SELECT word FROM lexicon;",sep="")
result<- dbSendQuery(con, statement = currQuery)
dataOut <- fetch(result, -1)
dbClearResult(dbListResults(con)[[1]])

wordList<-dataOut
wordCounts<-0
wordSums<-0

# pull out the list of dates, contained in headers of the first row
currQuery<-paste("SELECT * FROM lexicon LIMIT 1;",sep="")
result<- dbSendQuery(con, statement = currQuery)
nameFrame <- fetch(result, -1)
dbClearResult(dbListResults(con)[[1]])

### assuming existance of percentlexicon table, with first column word, add the successive name columns
for (colNo in seq(2,ncol(nameFrame))){
  currQuery<-paste("ALTER TABLE percentlexicon ADD COLUMN ", names(nameFrame)[colNo], " double precision;",sep="")
  dbSendQuery(con, statement = currQuery)
}


# get the sum of words in the month
for (p in seq(2,dim(nameFrame)[2])){
  currQuery<-paste("SELECT SUM(",names(nameFrame)[p],") FROM lexicon;",sep="")
  result<- dbSendQuery(con, statement = currQuery)
  wordSums[p] <- fetch(result, -1)
  dbClearResult(dbListResults(con)[[1]])
  
}

wordSums<-unlist(wordSums)

# keep this to something sensible to get a good 
# balance between speed and memory use
wordSection <- 10000

outEvery<-10

# table to hold output summed counts
wordCounts<- as.data.frame(setNames(replicate(ncol(nameFrame),numeric(0), simplify = F), letters[1:ncol(nameFrame)]))
names(wordCounts)<-names(nameFrame)

# query some amount of the table at a time
# not sure why I'm not going column by column...
for (p in seq(1:(dim(wordList)[1]%/%wordSection)+1)) {
  currQuery<-paste("SELECT * FROM lexicon LIMIT ", wordSection, " OFFSET ", p*wordSection,";",sep="")
  result<- dbSendQuery(con, statement = currQuery)
  wordGroup <- fetch(result, -1)
  wordGroup[is.na(wordGroup)]<-0
  dbClearResult(dbListResults(con)[[1]])
  
  # convert everything to fractional comp, then percentage
  wordGroup[,2:ncol(wordGroup)]<-sweep(wordGroup[,2:ncol(wordGroup)],2,wordSums[2:length(wordSums)],'/')
  wordGroup[,2:ncol(wordGroup)]<-wordGroup[,2:ncol(wordGroup)]*100
  
  theseWordCounts<- wordGroup
  names(theseWordCounts)<-names(nameFrame)
  
  dbWriteTable(con,name="percentlexicon",value=theseWordCounts,row.names=FALSE, append=TRUE)
  #wordCounts<-rbind(wordCounts,theseWordCounts)
  
  if (p-outEvery*p%/%outEvery==0){
    cat("word polled: ",p, "- index [", p,"] \n", sep="")
  }
}

dbDisconnect(con)


pw<-"disregard this"

print(proc.time()-ptm)

# fName<-paste("~/R/data/rcomments/redditLexiconTop10K.txt",sep="")
# write.table(topWords,fName,sep=",")
