library(RPostgreSQL)
ptm<- proc.time()

pw<-getPass("local")
drv<-dbDriver("PostgreSQL")
con<- dbConnect(drv,host="localhost",port=5432,dbname="fulllexicon",user="postgres",password=pw)

# pull off the list of words in the lexicon
currQuery<-paste("SELECT COUNT(*) FROM lexicon;",sep="")
result<- dbSendQuery(con, statement = currQuery)
dataOut <- fetch(result, -1)
dbClearResult(dbListResults(con)[[1]])

dbRows<-dataOut

# pull out the list of dates, contained in headers of the first row
currQuery<-paste("SELECT * FROM lexicon LIMIT 1;",sep="")
result<- dbSendQuery(con, statement = currQuery)
nameFrame <- fetch(result, -1)
dbClearResult(dbListResults(con)[[1]])

dateSeq<-seq(as.Date("2007-10-15"),as.Date("2016-02-22"),"weeks")

# keep this to something sensible to get a good 
# balance between speed and memory use
wordSection <- 10000

outEvery<-10

sumPercentThresh<-0.000000000000001
varThresh<-1e-7

# table to hold output summed counts
filteredWords<- as.data.frame(setNames(replicate(ncol(nameFrame),numeric(0), simplify = F), letters[1:ncol(nameFrame)]))
names(filteredWords)<-names(nameFrame)

# query some amount of the table at a time
# not sure why I'm not going column by column...
for (p in seq(1:(dim(wordList)[1]%/%wordSection-1))) {
  currQuery<-paste("SELECT * FROM percentlexicon LIMIT ", wordSection, " OFFSET ", p*wordSection,";",sep="")
  result<- dbSendQuery(con, statement = currQuery)
  wordGroup <- fetch(result, -1)
  wordGroup[is.na(wordGroup)]<-0
  dbClearResult(dbListResults(con)[[1]])
  
  
  theseFilteredWords<-wordGroup[(which(rowSums(wordGroup[,2:ncol(wordGroup)])>=sumPercentThresh)),]
  
  #bigVarWordInd<-which(apply(as.matrix(theseFilteredWords[,2:ncol(theseFilteredWords)]),1,var)>=varThresh)
  
  # look for words with large variance in the differential
  bigVarWordInd<-which(apply(apply(as.matrix(theseFilteredWords[,2:ncol(theseFilteredWords)]),1,diff),2,var)>=varThresh)
  
  if (length(bigVarWordInd)>0){
    theseFilteredWords<-theseFilteredWords[bigVarWordInd,]
    filteredWords<-rbind(filteredWords,theseFilteredWords)
  }
  
  if (p-outEvery*p%/%outEvery==0){
    cat("word polled: ",p, "- index [", p,"] \n", sep="")
  }
}

dbDisconnect(con)

filteredWordsUniq<-filteredWords[seq_along(filteredWords[,1])[!duplicated(filteredWords[,1])],]

write.table(filteredWordsUniq,file="~/R/data/rcomments/filteredWords.txt")

extraFilter<-filteredWordsUniq[which(apply(as.matrix(filteredWordsUniq[,2:ncol(filteredWordsUniq)]),1,var)>=varThresh*100),]
superFilter<-filteredWordsUniq[which(apply(as.matrix(filteredWordsUniq[,2:ncol(filteredWordsUniq)]),1,var)>=varThresh*1000),]


write.table(extraFilter,file="~/R/data/rcomments/filteredWords100xVar.txt")
write.table(superFilter,file="~/R/data/rcomments/filteredWords1000xVar.txt")

trumpFrame<-data.frame(dateSeq,t(rev(filteredWordsUniq[which(filteredWordsUniq[,1]=="trump"),2:ncol(filteredWordsUniq)])))
names(trumpFrame)<-c("date","percentage")

pw<-"disregard this"

print(proc.time()-ptm)

# fName<-paste("~/R/data/rcomments/redditLexiconTop10K.txt",sep="")
# write.table(topWords,fName,sep=",")
