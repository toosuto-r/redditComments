library(RPostgreSQL)
setwd("~/R/data")
##connect to db and return test results

ptm<-proc.time()

#get pass
pw<-getPass()
drv<-dbDriver("PostgreSQL")
con<- dbConnect(drv,host="remote.picodoc.org",port=12360,dbname="reddit",user="reddit",password=pw)

# get the dates used in the db
dateQuery<-"SELECT DISTINCT date FROM word_counts_recent ORDER BY date DESC"
dateResult<- dbSendQuery(con, statement = dateQuery)
datesUsed<-fetch(dateResult,-1)
dbClearResult(dbListResults(con)[[1]])

#create a table of the dimensions appropriate to hold 300 subs for each date
topSubTable<-as.data.frame(matrix(nrow=300,ncol=dim(datesUsed)[1]))


for (p in seq(1,dim(datesUsed)[1])){
currDate<-datesUsed[p,]
currQuery<-paste("SELECT subreddit, SUM(count) from word_counts_recent WHERE date='",currDate,"'GROUP BY subreddit ORDER BY sum DESC LIMIT 300",sep="")
result<- dbSendQuery(con, statement = currQuery)
dataOut <- fetch(result, -1)
dbClearResult(dbListResults(con)[[1]])
topSubTable[,p]<-dataOut$subreddit
}
dbDisconnect(con)

names(topSubTable)<-datesUsed$date

fName<-"C:/Users/Ryan/Documents/R/data/rcomments/topSubs300-dates_recent.txt"
write.table(topSubTable,fName,sep=",")

print(proc.time()-ptm)
