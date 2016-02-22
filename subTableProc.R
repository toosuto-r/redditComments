#scan through each word for each subreddit, and match it to each occurrence of that word in other subs.
setwd("~/R/data")


#read the list of top subreddits, which have been polled for top word lists recorded as text files
subs<-read.table(file="F:/topSubs300")
subLim<-nrow(subs)

#create a table to hold the links between each sub (new subs will be seq. appended)
# links<-data.frame(sub=double())
links<-data.frame(matrix(NA, nrow = subLim, ncol = subLim))
subSize<-data.frame(sub=double(), size=double())


#loop through all the sub files
for (p in seq(1,subLim-1)){
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
  for (q in seq(p+1,subLim)){
    currScanSub<-as.character(subs[q,1])
    scanFileLoc<-paste("~/R/data/rcomments/",currScanSub,".txt",sep="")
    scanRed<-read.table(file=scanFileLoc,header=TRUE, sep=",")
    
    #create an empty logical array for each word in the file. This is redefined with every new scan sub
    wordsScanned<-logical(wordLim)
    #get total number of word entries
    nEntries<-sum(scanRed$nentry)
    
    #set likeness score counter for this pair to zero
    currLikeness=0

    #loop through each word in the current subreddit and compare it to the words in the next sub
    for (r in seq(1,wordLim)){
      #identify the current word
      currWord<-as.character(currRed[r,1])
      #and its percentage use
      currPer<-currRed[r,3]/nMainEntries*100
      #identify the matching word position in the scan sub, and get its percentage use
      wordBool<-scanRed[,1]==currWord
      scanPer<-scanRed[wordBool,3]/nEntries*100

      
      #note the words that have been used in the scan sub
      wordsScanned[wordBool]<-TRUE
      
      #check if the word has a match - if not, the word takes the maximum value it could have
      #if the other word was in position 3001
      if (all(!wordBool)){
        currLikeness<-currLikeness+currPer^2

      }
      else{
        currLikeness<-currLikeness+(currPer+scanPer)*abs(currPer-scanPer)
      }
    
    }
    
    
    #identify unused words and add the score from unused words in the scan sub
    unusedNentries<-scanRed$nentry[!wordsScanned]/nEntries
    currLikeness<-currLikeness+sum(unusedNentries^2)
    
    
    #place the currLikeness into [p,q] of the relational table
    print(currLikeness)
    links[p,q]<-currLikeness
    cat(p,q)
  }
  
}

names(links)<-subs$subreddit
row.names(links)<-subs$subreddit

fName<-"F:/Data/rcomments/subRelation.txt"
write.table(links,fName,sep=",")

fName2<-"C:/Users/Ryan/Documents/R/data/rcomments/subSizes.txt"
write.table(subSize,fName2,sep=",")




