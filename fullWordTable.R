# get the full word table from the top 300 subreddits
# open first file, use this for base words, then add 
# match these back into the main table, add if they match, append the ones that don't
ptm<-proc.time()


#read the list of top subreddits, which have been polled for top word lists recorded as text files
subs<-read.table(file="C:/Users/Ryan/Documents/R/data/rcomments/topSubs300.txt")
subLim<-nrow(subs)


# get the list of words from the first sub, which will be used as the population base
firstSub<-as.character(subs[1,1])
firstFileLoc<-paste("~/R/data/rcomments/",firstSub,".txt",sep="")
mainRed<-read.table(file=firstFileLoc,header=TRUE,sep=",")

# scan through the rest of the subs and match words
for (p in seq(2,subLim)){
  currSub<-as.character(subs[p,1])
  fileLoc<-paste("~/R/data/rcomments/",currSub,".txt",sep="")
  currRed<-read.table(file=fileLoc,header=TRUE, sep=",")
  
  # add word counts which already exist to the current count
  addedWords<-match(currRed$word,mainRed$word)
  mainRed[na.omit(addedWords),3]<-mainRed[na.omit(addedWords),3]+currRed[!is.na(addedWords),3]
  
  # add extra words to a frame to be appaended to the end of the current frame
  extraWords<-currRed[is.na(addedWords),1]
  extraNums<-currRed[is.na(addedWords),3]
  
  extraTable<-data.frame(extraWords,rep(0,length(extraWords)),extraNums)
  names(extraTable)<-names(mainRed)
  
  # bind the tables together to be ssearched in the next iteration
  mainRed<-rbind(mainRed,extraTable)
    
}

fullWords<-mainRed[order(-mainRed$nentry),]

writeFName<-"C:/Users/Ryan/Documents/R/data/rcomments/fullWords.txt"
write.table(fullWords,writeFName,sep=",")



print(proc.time()-ptm)