#scan through each word for each subreddit, and match it to each occurrence of that word in other subs.
setwd("~/R/data")
ptm<-proc.time()


#read the list of top subreddits, which have been polled for top word lists recorded as text files
subs<-read.table(file="C:/Users/Ryan/Documents/R/data/rcomments/topSubs300.txt")
subLim<-nrow(subs)
# subLim<-7
# read the full word frequency table from file
readFName<-"C:/Users/Ryan/Documents/R/data/rcomments/fullWords.txt"
fullWord<-read.table(file=readFName,header=TRUE, sep=",")


# remove the "delet" entry, for it is poison
fullWord<-fullWord[-match("delet",fullWord$word),]


fullWordnEntry<-sum(fullWord$nentry)


#create a table to hold the links between each sub (new subs will be seq. appended)
# links<-data.frame(sub=double())
links<-data.frame(matrix(NA, nrow = subLim, ncol = subLim))
subSize<-data.frame(sub=double(), size=double())
likenessTable<-data.frame(wordCurr=double(),wordScan=double(),strength=double())
# maxLikeness<-data.frame(sub1=double(),sub2=double(),strength=double())
minLikeness<-data.frame(strength=double())
count<-0

#loop through all the sub files
for (p in seq(1,subLim-1)){
  currSub<-as.character(subs[p,1])
  fileLoc<-paste("~/R/data/rcomments/",currSub,".txt",sep="")
  
  currRed<-read.table(file=fileLoc,header=TRUE, sep=",")
  #clear "delet"
  currRed<-currRed[-(match("delet",currRed$word)),]
  
  wordLim<-nrow(currRed)
  
  #number of word entries in the main sub
  nMainEntries<-sum(currRed$nentry)
  subSize[p,1]<-currSub
  subSize[p,2]<-nMainEntries

  # go through each of the remaining subreddit files
  for (q in seq(p+1,subLim)){
    currScanSub<-as.character(subs[q,1])
    scanFileLoc<-paste("~/R/data/rcomments/",currScanSub,".txt",sep="")
    scanRed<-read.table(file=scanFileLoc,header=TRUE, sep=",")
    #clear "delet"
    scanRed<-scanRed[-(match("delet",scanRed$word)),]
    
    #get total number of word entries for the scan sub
    nEntries<-sum(scanRed$nentry)
    

    #set likeness score counter for this pair to zero
    currLikeness=0
    
    #loop through each word in the current subreddit and compare it to the words in the next sub

    # find where the current words are in the scan sub

    wordInd<-match(currRed$word,scanRed$word)
    
    # find the position of scan words in the current sub list
    scanWordInd<-match(scanRed$word,currRed$word)
    

    
    ## this is discarding NAs at the start - possibly faster but very very little
    
    # get the percentage use in the main sub of the words which have been found in both
    # currPer<-currRed[!is.na(wordInd),3]/nMainEntries*100
    
    # get the percentage use of thoses words in the scan sub  
    # scanPer<-scanRed[na.omit(wordInd),3]/nEntries*100
    # nullCurrPer<-currRed[is.na(wordInd),3]/nMainEntries*100
    # nullScanPer<-scanRed[is.na(scanWordInd),3]/nEntries*100
    
    ##
    #     currPer<-currRed[,3]/nMainEntries*100
    #     scanPer<-scanRed[(wordInd),3]/nEntries*100
    #     nullCurrPer<-currRed[is.na(wordInd),3]/nMainEntries*100
    #     nullScanPer<-scanRed[is.na(scanWordInd),3]/nEntries*100
    
    ######### JUST SET THE CURRPER AS ALL OF ITS PERS, THEN THE SCANPER IS 
    # THE RE-ORDERED PERCENTAGES TO MATCH, WITH ZEROES WHERE THERE IS NO MATCH
    # AND TACK THE MISSING SCANRED WORDS ON THE END WITH MATCHING ZEROES IN THE CURRRED

    # get the current percentage use of words in the main sub, and the 
    # percentage use of unmatched words in the scan sub (i.e. 0% use) 
    # using the matched indices for scanRed orders the table so each
    # row of currPer and scanPer address the same word, indexed by currPer

    currPer<-currRed[,3]/nMainEntries*100
    scanPer<-scanRed[wordInd,3]/nEntries*100
    scanPer[is.na(scanPer)] <- 0
    
    # get the matching words for the weights
    currWords<-currRed[,1]
    scanWords<-scanRed[is.na(scanWordInd),1]
    
    #then find the NAs in the scan match, put them in a new vector, 
    # match it with zeroes of the same length and cat both on the end
    nullScanPer<-scanRed[is.na(scanWordInd),3]/nEntries*100
    nullScanPerMatch<-rep(0,length(nullScanPer))
    
    currPer<-c(currPer,nullScanPerMatch)
    scanPer<-c(scanPer,nullScanPer)
   
    #combine the matching words
    currFullWord<-c(as.vector(currWords),as.vector(scanWords))
    
    # calculate the weights, modifying the difference by adding 1 
    # to ensure the resulting product is larger than the starting terms  
    weights<-(abs(currPer-scanPer)+1)*(currPer+scanPer)
    currLikeness<-sum(weights,rm.na=TRUE)
    
    ############################
    # to find the true differential between words, we look for a word which is responsible 
    # for a very small difference between the two (subsDifference), but is also substantially
    # different from the main reddit corpus (mainDifference). The mainDifference should be the
    # over- or under-representation of the word in the main corpus.
    # The metric is subsDifference/mainDifference
    # weight*currUse/mainCorpusUse? 
    # weight/((currUse-mainCorpusUse)*currUse) - big when it's close to main use, small when far away
    #                                   and also smaller for higher-use words (currUse<- (currPer+scanPer)/2)
    
    ############################
    
    currUse<-(currPer+scanPer)/2
    currFullPer<-fullWord[match(currFullWord,fullWord$word),3]/fullWordnEntry*100
    # linksStrength<-weights/(((currUse-currFullPer))*(currUse))
    linksStrength<-((abs(currUse-currFullPer)+1)*(currUse)^1)/weights
    # linksStrength<-(((currUse-currFullPer)+1)*currUse)/weights
    # linksStrength<-((currUse-currFullPer)+1)/(abs(currPer-scanPer))^2
    # linksStrength<-((currUse-currFullPer)+1)*currUse^3/(abs(currPer-scanPer))
    
    #place the currLikeness into [p,q] of the relational table
    # print(currLikeness)
    links[p,q]<-currLikeness
    # cat(p,q)
    count<-count+1

    currPer<-0
    scanPer<-0
    weights<-0

    minInd<-which.max(abs(linksStrength))
    minLikeness[count,1]<-currFullWord[minInd]
    # print(minInd)

  }
  
  
}



names(links)<-subs$V1
row.names(links)<-subs$V1

fName<-"C:/Users/Ryan/Documents/R/data/rcomments/subRelation-re3.txt"
write.table(links,fName,sep=",")

fName2<-"C:/Users/Ryan/Documents/R/data/rcomments/subSizes-re.txt"
write.table(subSize,fName2,sep=",")

fName3<-"C:/Users/Ryan/Documents/R/data/rcomments/minConnection3.txt"
write.table(minLikeness,fName3,sep=",")

print(proc.time()-ptm)