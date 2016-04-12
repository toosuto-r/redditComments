library(RPostgreSQL)
library(plyr)

setwd("~/R/data")
##connect to db and return test results

ptm<-proc.time()

#get pass
pw<-getPass()
drv<-dbDriver("PostgreSQL")
con<- dbConnect(drv,host="remote.picodoc.org",port=12360,dbname="reddit",user="reddit",password=pw)

# get the dates used in the db
dateQuery<-"SELECT DISTINCT date FROM word_counts ORDER BY date DESC"
dateResult<- dbSendQuery(con, statement = dateQuery)
datesUsed<-fetch(dateResult,-1)
dbClearResult(dbListResults(con)[[1]])

cat("time elapsed for date fetch: ",(proc.time()-ptm)[3],"s.\n",sep="")
#create a table of the dimensions appropriate to hold 300 subs for each date

ptm<-proc.time()

topSubTable<-as.data.frame(matrix(nrow=300,ncol=dim(datesUsed)[1]*2))



for (p in seq(1,dim(datesUsed)[1])){
currDate<-datesUsed[p,]
currQuery<-paste("SELECT subreddit, SUM(count) from word_counts WHERE date='",currDate,"'GROUP BY subreddit ORDER BY sum DESC LIMIT 300",sep="")
result<- dbSendQuery(con, statement = currQuery)
dataOut <- fetch(result, -1)
dbClearResult(dbListResults(con)[[1]])

if (dim(dataOut)[1]<=300) {
  extender<-as.data.frame(matrix(nrow=300-dim(dataOut)[1],ncol=2))
  names(extender)<-names(dataOut)
  dataOut<-rbind(dataOut,extender)
}

topSubTable[,(p*2)-1]<-dataOut$subreddit
topSubTable[,(p*2)]<-dataOut$sum

}
dbDisconnect(con)

names(topSubTable)<-datesUsed$date

fName<-"C:/Users/Ryan/Documents/R/data/rcomments/topSubs300-dates_full.txt"
write.table(topSubTable,fName,sep=",")

cat("time elapsed for top sub fetch: ",(proc.time()-ptm)[3],"s.\n",sep="")
