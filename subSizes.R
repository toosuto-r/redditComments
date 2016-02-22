#scan through each word for each subreddit, and match it to each occurrence of that word in other subs.
setwd("~/R/data")


#read the list of top subreddits, which have been polled for top word lists recorded as text files
subs<-read.table(file="C:/Users/Ryan/Documents/R/data/rcomments/topSubs300.txt")
subLim<-nrow(subs)

#create a table to hold the links between each sub (new subs will be seq. appended)
# links<-data.frame(sub=double())
links<-data.frame(matrix(NA, nrow = subLim, ncol = subLim))
subSize<-data.frame(sub=double(), size=double())


#loop through all the sub files
for (p in seq(1,subLim)){
  currSub<-as.character(subs[p,1])
  fileLoc<-paste("~/R/data/rcomments/",currSub,".txt",sep="")
  
  currRed<-read.table(file=fileLoc,header=TRUE, sep=",")
  wordLim<-nrow(currRed)
  
  #number of word entries in the main sub
  nMainEntries<-sum(currRed$nentry)
  subSize[p,1]<-currSub
  subSize[p,2]<-nMainEntries
  
  #for determining value of missing words with booleans - maybe better to select from scanRed$nentry?
  # nWordInd<-seq(1,wordLim)
  
  
  # go through each of the remaining subreddit files
}

# fName<-"F:/Data/rcomments/subRelation.txt"
# write.table(links,fName,sep=",")

fName2<-"C:/Users/Ryan/Documents/R/data/rcomments/subSizes.txt"
write.table(subSize,fName2,sep=",")




