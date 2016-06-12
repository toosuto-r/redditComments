#import the list of subreddits from before 

library(RPostgreSQL)

pw<-getPass("local")

fName<-paste("~/R/data/rcomments/lexicon_main.txt",sep="")
mainLexicon<-read.table(fName,sep=",",header = TRUE, check.names=FALSE)
#write.table(mainLexicon,fName,sep=",", quote = FALSE, row.names=FALSE, col.names = FALSE)

drv<-dbDriver("PostgreSQL")
con<- dbConnect(drv,host="localhost",port=5432,dbname="fulllexicon",user="postgres",password=pw)

for (colNo in seq(2,length(names(mainLexicon)))){

currQuery<-paste("ALTER TABLE lexicon ADD COLUMN w", gsub("-","",names(mainLexicon)[colNo]), " integer;",sep="")
result<- dbSendQuery(con, statement = currQuery)
#dataOut <- fetch(result, -1)
dbClearResult(dbListResults(con)[[1]])
}

write.table(mainLexicon, file="~/R/data/rcomments/lexicon_main_noNames.csv (DELIMITER ',')",row.names = FALSE, col.names = FALSE, na="0", sep=",")

currQuery<-paste("\COPY lexicon FROM 'C:/Users/Ryan/Documents/R/data/rcomments/lexicon_main_noNames.csv' (DELIMITER ',')")
result<- dbSendQuery(con, statement = currQuery)
dbClearResult(dbListResults(con)[[1]])

# words<-c("aa","bb","cc","dd")
# counts<-c(5,3,8,6)
# testTable<-data.frame(words,counts)

dbWriteTable(con,name="testWordTable",value=testTable,row.names=FALSE, overwrite=TRUE)
