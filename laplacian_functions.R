library(RANN)
library(Matrix)
library(rARPACK)
library(DMwR)
library(data.table)

compute_weightedgraph <- function(X, k, epsilon){
  # Constructs symmetric adjacency graph using KNN, 
  # returns sparse weight matrix based on distance
  #
  #Args:
  #  X: data matrix
  #  k: number of nearest neighbors to use for adjacency graph
  #  epsilon: epsilon parameter of kernel used to calculate weights:
  #           w = exp(-D$^2$/(epsilon))
  #  
  #Returns:
  #  W: weight matrix
  
  #KNN: xnn$nn.idx has dim (nrow(X), k), where k is the index of the NN
  n <- nrow(X)
  if (missing(k)) k = 0.01*n
  xnn <- nn2(data=X, query=X, k=(k+1), treetype='kd', searchtype='standard', eps=0.0)
  rm(X)
  #1st NN node returned for x_i is just x_i itself, not used:
  xnn$nn.idx <- xnn$nn.idx[,-1] 
  xnn$nn.dists <- xnn$nn.dists[,-1]
  
  #Create a sparse weight matrix. {xnn$nn.ind_ij}: for each point x_i, j gives the indices of the kNN. There it is filled with the corresponding weight calculated using {xnn$nn.dists_ij}. See ?sparseMatrix: row, col specify the location of non-zero entries of the matrix, x specifies their values. 
  #Weight matrix is symmetric: nodes i and j are connected by an edge if i is among the k nearest neighbors of j, or if j is among the k nearest neighbors of i.
  row <- c(rep(1:n, k), c(xnn$nn.idx))
  col <- c(c(xnn$nn.idx), rep(1:n, k))
  W <- exp(-c(xnn$nn.dists, xnn$nn.dists)^2/epsilon)
  rm(xnn); gc() #garbage collection
  W <- sparseMatrix(i=row, j=col, x=W, use.last.ij=T) #use the last weights for duplicated pairs (row, col)
  
  return(W)
}

rwlaplacian <- function(X, k, epsilon, W, neigens=20){
  #Computes eigenvectors of random walk Laplacian
  #
  #Args:
  #  X: data matrix
  #  k: number of nearest neighbors to use for adjacency graph
  #  epsilon: epsilon parameter of kernel used to calculate weights: 
  #           w = exp(-D$^2$/(epsilon))
  #  W: weight matix, optional. If specified k and epsilon not needed.
  #Returns:
  #  L: Laplacian matrix
  #  W: weight matrix
  #  eigens: eigenvalues/vectors
  
  if (missing(W)){
    W <- compute_weightedgraph(data=X, k=k, epsilon=epsilon)
  } 

  Dinv = Diagonal(x = 1/rowSums(W))
  L = Dinv %*% W
  eigens = eigs(L, neigens, which = 'LR')

  return(list(L, W, eigens))
}

#############################################################

weightedfeature <- function(train, w, idx, i, smotedX, k){
  nnx <- train[idx, i]
  dim(nnx)  <- c(nrow(smotedX), k)
  x <- rowSums(w*nnx)
  return(x)
}

rwlsmote <- function(data, embedding, y, perc.over=200, minority = 1, majority=0, k=30, epsilon, neigens=20){
  #Random Walk Laplacian Synthetic Minority Oversampling Technique (SMOTE)
  #Maps data to embedding given by the random walk Laplacian, oversamples 
  #with SMOTE, and maps synthetic points back to original space using kNN
  #
  #Args:
  #  data: original data with the response
  #  embedding: random walk Laplacian embedding
  #  y: binary response vector for the embedding
  #  
  #Returns:
  #  Dataset with new synthetic points
  
  rwltrain <- data.table(embedding, class=y)
  rwltrain1 <- rwltrain[rwltrain$class == minority,]
  rwltrain0 <- rwltrain[rwltrain$class == majority,]
  rwltrain <- rbind(rwltrain1, rwltrain0)
  rwltrain$class <- factor(rwltrain$class)
  
  smotedX <- SMOTE(class ~ ., perc.over=perc.over, perc.under=0, data=rwltrain)
  smotedX <-smotedX[nrow(rwltrain1):nrow(smotedX),] #First nrow(rwltrain1) are original points, get synthetic data only
  
  #look through transformed training data to find NN of new smoted points. 
  smotenn <- nn2(data=rwltrain[,!'class', with=F], query=smotedX, k=k, treetype="kd", searchtype='standard', eps=0.0)
  
  #Reconstruct synthetic nodes using training nodes
  D = rowSums(smotenn$nn.dists)
  w = smotenn$nn.dists/D
  
  #Reconstruct synthetic points using original points
  data <- data.frame(data)
  Xw <- sapply(1:(ncol(data)-1), function(i) weightedfeature(train=data, w=w, idx=smotenn$nn.idx, i, smotedX=smotedX, k=k))
  newdata <- data.frame(cbind(Xw, y=rep(minority, nrow(smotedX))))
  names(newdata) <- names(data)
  trainrwlsmote <- rbind(data, newdata)
}