getPass<- function(location){
  if (location=="local"){
    pw<-read.csv(file="C:/postgres/pw.txt", header = FALSE)
  } else {
    pw<-read.csv(file="C:/Users/Ryan/Documents/R/data/pw_remote.txt", header = FALSE)
  }
  pw<-as.character(pw[1,1])
  return(pw)
}

