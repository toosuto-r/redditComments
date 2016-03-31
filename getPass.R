getPass<- function(){
  pw<-read.csv(file="C:/Users/Ryan/Documents/R/data/pw_remote.txt", header = FALSE)
  pw<-as.character(pw[1,1])
  return(pw)
}

