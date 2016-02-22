library(RPostgreSQL)
setwd("~/R/data")

##connect to db and return test results

#get pass
pw<-getPass()

#get the list of subreddits and their words
subs<-read.table(file="C:/Users/Ryan/Documents/R/data/rcomments/topSubs300.txt")
lim=nrow(subs)

for (p in seq(1,lim)){
  
  #define the query to return the pop table
  currSub<-as.character(subs[p,1])
  currQuery<-paste("SELECT * FROM ts_stat($$SELECT tsv FROM cproc WHERE subreddit='",currSub ,"' $$) ORDER BY nentry DESC, ndoc DESC, word LIMIT 3000;",sep="")
  
  #connect to main comment database
  con<- dbConnect(PostgreSQL(), user="postgres",password=pw,dbname="rcomments")
  
  result<- dbSendQuery(con, statement = currQuery)
  dataOut <- fetch(result, 3000)
  dbClearResult(dbListResults(con)[[1]])
  dbDisconnect(con)
  
  #write the pop table for the current sub
  fName<-paste("C:/Users/Ryan/Documents/R/data/rcomments/",currSub,".txt",sep="")
  write.table(dataOut,fName,sep=",")
}

