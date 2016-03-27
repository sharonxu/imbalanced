
# This file performs 5-fold cross-validation on a dataset using a number of oversampling and classification methods. Currently, it is set to perform RWL-SMOTE and radius NN on the loans data.

# To make changes to this file, simply uncomment the classification method you wish to run, and source.

#X is the dataset (or PCA/Laplacian embedding) containing the response y
#y is the response vector, saved separately as well

library(caret)
#set working directory to source functions and load data:
dir <- 'C:/Users/Sharon/Dropbox/RWL_SMOTE' 
setwd(dir)
source(paste(dir, '/src/classification_functions.R', sep=''))
source(paste(dir, '/src/laplacian_functions.R', sep=''))

load('L_embedding.RData') #loans Laplacian embedding, contains Xrwl and y
X <- data.frame(Xrwl, y)
loans <- readRDS('scaled_loans.rds') #original normalized loans data with y

#Separate data into: 
#1. X = (Xrwl, y) used for training best parameters for RWL-SMOTE
#Want test data to have similar distribution as original 
#2. Xtest for gains chart

ind1 <- which(X$y == 1)
ind0 <- which(X$y == 0)

length(ind1)/nrow(X)
perc_minority <- 0.05
ntest1 <- ceiling(perc_minority*0.2*nrow(X))
ntest0 <- ceiling((1 - perc_minority)*0.2*nrow(X))

testind1 <- sample(ind1, ntest1)
testind0 <- sample(ind0, ntest0)

#Option A:
#RWL-SMOTE parameters:
perc=150
ksmote = 100

#KNN parameter
k = 10
threshold=NULL
train <- rwlsmote(data=loans[-c(testind1, testind0),], embedding=Xrwl[-c(testind1, testind0),], y=y[-c(testind1, testind0)], perc.over=perc, k=ksmote)
test <- loans[c(testind1, testind0),]
pred <- knn.nn2(train=train, test=test, k=k, threshold=threshold)

library(gbm)
gbm1 <- gbm(y~., data=train, n.trees=200, distribution = "adaboost")

evalResults <- data.frame(Class = factor(test$y))
evalResults$rwlsmote_knn <- 1 - pred
evalResults$rwlsmote_knn <- 1 - predict(gbm1, newdata=test, type="response")

head(evalResults)
liftData <- lift(Class ~ rwlsmote_knn, data = evalResults)
plot(liftData, values = 60, auto.key = list(columns = 3,lines = TRUE, points = FALSE))

#Option B: Use 5-fold cross validation to determine best parameters with training data:

Xtest <- X[c(testind1, testind0),]
X <- X[-c(testind1, testind0),]

set.seed(8)
foldind <- createFolds(X$y, k=5) #random indices for each fold
folds <- lapply(foldind, function(ind, data) data[ind,], data = X) #list of partitioned datasets
unlist(lapply(folds, nrow))


#5-fold cross validation 
recall <- numeric(0); precision <- numeric(0); tablesum <- 0 #initialize metrics
for (i in 1:5){
  train <-  do.call("rbind", folds[-i]) #all but the ith fold make up training data
  test <- do.call("rbind", folds[i]) # ith fold makes up training data
  
  #Oversampling: SMOTE, RWL-SMOTE, and None
  ind <- do.call("c", foldind[-i]) #get indices of training data
  
  warning('MODIFY Xrwl!!!')
  train <- rwlsmote(data=train, embedding=Xrwl[ind,], y=y[ind], perc.over=perc, k=ksmote)

  #train$y <- as.factor(train$y); test$y <- as.factor(test$y)
  #train <- SMOTE(y~., data=train, perc.over=perc)
  
  #Classification Functions:
  #train$y <- as.integer(as.character(train$y)); test$y <- as.integer(as.character(test$y)) #run if SMOTE is performed to convert factors back to integers
  
  pred <- knn.nn2(train=train, test=test, k=k, threshold=threshold)

  #Baseline: SVM
  #train$y <- as.factor(train$y); test$y <- as.factor(test$y)
  #model <- svm(y ~ ., data = train, kernel='radial')
  #pred <- predict(model, test)
  
  table1 <- table(pred, test$y)
   
  evalResults <- data.frame(Class = factor(test$y))
  evalResults$rwlsmote_knn <- 1 - pred
  head(evalResults)
  
  trellis.par.set(caretTheme())
  liftData <- lift(Class ~ rwlsmote_knn, data = evalResults)
  plot(liftData, values = 60, auto.key = list(columns = 3,lines = TRUE, points = FALSE))
  
  confmatrix <- confusionMatrix(table1, positive='1')
  recall <-  c(recall, confmatrix$byClass[1])
  precision  <-  c(precision, confmatrix$byClass[3])
  tablesum <- table1+tablesum
}



avgprecision = mean(precision, na.rm=T)
avgrecall  = mean(recall, na.rm=T)
sdprecision = sd(precision, na.rm=T)
sdrecall  = sd(recall, na.rm=T)
f1 = 2*precision*recall/(precision+recall)
avgf1 = mean(f1, na.rm=T)
sdf1 = sd(f1, na.rm=T)

avgprecision; sdprecision
avgrecall; sdrecall
avgf1; sdf1
tablesum
