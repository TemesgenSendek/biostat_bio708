d_stat <- function(vector){
  mn <- mean(vector)
  md <- median(vector)
  return(c(mn, md))
}
d_stat(c(1:15))

mylist<-list(A=matrix(1:9,nrow=3),B=1:5,C=8)
mylist
d <- lapply(mylist, function(x) x*20)
d$A
