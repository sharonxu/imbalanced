
split_data <- function(x, y, minority=1, majority=0, train_fraction, seed){
  data <- data.frame(cbind(x, y))

  #Separate minority and majority classes. We take train_fraction of the observations from each class, to ensure the training and testing data have a comparable percentage of minority observations. (This is to account for the edge case of extremely imbalanced datasets, where a random split may result in no minority points in the test data).
  
#Shift train observations so that points 1,...,ntrain1 in the training data are the minority class (class 1). This will simplify nearest neighbor classification with nn2:
  
  data1 <- data[data$y == minority, ]
  data0 <- data[data$y == majority, ]
  
  set.seed(seed)
  #Minority points:
  trainind1 <- sample(nrow(data1), train_fraction*nrow(data1))
  train1 <- data1[trainind1,]
  test1 <-  data1[-trainind1,]
  #Majority points:
  trainind0 <- sample(nrow(data0), train_fraction*nrow(data0))
  train0 <- data0[trainind0,]
  test0 <-  data0[-trainind0,]
  
  ntrain1 <- nrow(train1)
  ntrain0 <- nrow(train0)
  train <- rbind(train1, train0)
  test <- rbind(test1, test0)

  return(list(train, test, ntrain1, ntrain0, trainind1, trainind0))
}

#Enables function to return multiple objects instead of a list of objects. From the development version of gsubfn: https://raw.githubusercontent.com/ggrothendieck/gsubfn/master/R/list.R
list <- structure(NA,class="result")
"[<-.result" <- function(x,...,value) {
  args <- as.list(match.call())
  args <- args[-c(1:2,length(args))]
  length(value) <- length(args)
  for(i in seq(along=args)) {
    a <- args[[i]]
    if(!missing(a)) eval.parent(substitute(a <- v,list(a=a,v=value[[i]])))
  }
  x
}
