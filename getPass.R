getPass<- function(){
  pw<-read.csv(file="C:/Users/Ryan/Documents/R/data/pw.txt", header = FALSE, sep=",")
  pw<-as.character(pw[1,1])
  return(pw)
}