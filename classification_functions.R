library(RANN)

calculateFraction <- function(r) {
  
  #helper function: calculates the minority fraction for plotting
  x <- nn2(data=nntrain, query=nntest, k=nrow(nntrain), treetype="kd", searchtype="radius", radius=r, eps=0.0)$nn.idx
  y <- mean(apply(x <= n1 & x != 0, 1, sum)/apply(x != 0, 1, sum), na.rm=TRUE) # y is a fraction
  return(y)
}

knn.nn2 <- function(train, test, response = 'y', minority = 1, majority=0, k=5, plot=F, threshold = NULL){
  # Computes the fraction of minority points over total points within a certain radius of each test point
  # Each test point is then assigned to the minority class if it exceeds the specified threshold
  #
  #Args:
  # train: train data matrix
  # test: test data matrix
  # response: name of response column for train and test
  # minority: numeric value for minority class
  # majority: numeric value for majority clas
  # k: number of neighbors to find
  # threshold: numeric value between 0 and 1. Used to decide if test point is predicted as minority or majority class
  #            (as the minority fraction is bounded between 0 and 1)
  #
  #Returns:
  # the predicted class (minority or majority) of each row of test data
  
  response <- which(names(train)==response) #get column index of response
  train1 <- train[train[,response]==minority,]
  train0 <- train[train[,response]==majority,]
  n1 <- nrow(train1)
  train <- rbind(train1, train0)
  
  #Perform NN with radius search on projected coordinates 
    testnn <- nn2(data=train[, -response], query=test[, -response], treetype="kd", searchtype='standard', eps=0.0)
  
  #Predict the class of the test data.
  #Points 1,...,n1 in the training data are the minority class. For each test point, count the indices of nearest neighbors (NN) in this range, and divide by total number of NN.
  
  nnk <- apply(testnn$nn.idx != 0, 1, sum)
  minorityfraction <- apply(testnn$nn.idx <= n1, 1, sum)/nnk
  
  if (is.null(threshold)){
    return(minorityfraction)
  }
  return (as.integer(minorityfraction >= threshold))
}

