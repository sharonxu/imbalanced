library(data.table)
library(caret)
#library(unbalanced)

dir <- 'C:/Users/Sharon/Dropbox/Math191'
setwd(dir)
source(paste(dir, '/src/utility_functions.R', sep=''))
source(paste(dir, '/src/laplacian_functions.R', sep=''))
source(paste(dir, '/src/classification_functions.R', sep=''))

#Loans data
# load(paste(dir, '/data/loans.data', sep=''))
# ind <- sapply(loans, function(x) sum(is.na(x))==0 & is.numeric(x))
# loans <- loans[,ind, with=F]
# loans <- loans[,!c('is.default2','train','acct_ind'), with=F]
# X <- data.table(scale(loans[,!'is.default', with=F]))
# y <- loans$is.default
# 
# #Informed undersampling of majority class
# data <- ubOSS(X=X, Y=y, verbose = TRUE) #Tomek links followed by CNN
# data <- ubNCL(X=data$X, Y=data$Y, verbose = TRUE)
# set.seed(3)
# data2 <- ubUnder(X=data$X, Y=data$Y, perc=10, method='percUnder')
# X <- data2$X; y <- data2$Y
# save(X, y, file='X_y_scaledloans.RData')
load('X_y_scaledloans.RData') 

#Yeast data
# load(paste(dir, '/data/yeast.RData', sep=''))
# yeast <- data.table(yeast)
# X <- data.table(scale(yeast[,!'negative', with=F]))
# y <- yeast$negative

#simulated data
# load('simulatedData.RData')
# X <- data.table(rbind(train, test))
# y <- train$v3
# X <- data.table(scale(X[,!'V3', with=F]))

### Map all points to the Laplacian embedding
epsilon=10.8; k=130
W <- compute_weightedgraph(X=X, epsilon=epsilon, k=k)
truehist(W@x, xlab='Weights', col='darkslategray3', main='Random Walk Laplacian Weights: Simulated Data', prob=T, nbins=50)
mtext(side = 1, text = paste('epsilon = ', epsilon, ', k = ', k, sep=''), line = 4, cex=0.9)
isSymmetric(W)

list[L, W, eigens] <- rwlaplacian(X=X, W=W, neigens=10)

barplot(height = eigens$values, ylab='Eigenvalues', col='darkslategray3', border='azure2', main='Random Walk Laplacian: Eigenvalue Decay for Simulated Data', ylim=c(0,1), cex.main=1.05)
mtext(side = 1, text = 'Index of Eigenvalues', line = 1)
mtext(side = 1, text = paste('epsilon =', epsilon, ', k =', k), line = 3, cex=0.8)

#Take top 5 nontrivial eigenvalues
m = 5
Xrwl <- t(eigens$values[2:(m+1)]*t(eigens$vectors[,2:(m+1)])) #our embedding
