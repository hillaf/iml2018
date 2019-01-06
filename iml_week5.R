# IML week 5

# 3 b

# Load the MNIST digit recognition dataset into R
# http://yann.lecun.com/exdb/mnist/
# assume you have all 4 files and gunzip'd them
# creates train$n, train$x, train$y  and test$n, test$x, test$y
# e.g. train$x is a 60000 x 784 matrix, each row is one digit (28x28)
# call:  show_digit(train$x[5,])   to see a digit.
# brendan o'connor - gist.github.com/39760 - anyall.org

load_mnist <- function() {
  load_image_file <- function(filename) {
    ret = list()
    f = file(filename,'rb')
    readBin(f,'integer',n=1,size=4,endian='big')
    ret$n = readBin(f,'integer',n=1,size=4,endian='big')
    nrow = readBin(f,'integer',n=1,size=4,endian='big')
    ncol = readBin(f,'integer',n=1,size=4,endian='big')
    x = readBin(f,'integer',n=ret$n*nrow*ncol,size=1,signed=F)
    ret$x = matrix(x, ncol=nrow*ncol, byrow=T)
    close(f)
    ret
  }
  load_label_file <- function(filename) {
    f = file(filename,'rb')
    readBin(f,'integer',n=1,size=4,endian='big')
    n = readBin(f,'integer',n=1,size=4,endian='big')
    y = readBin(f,'integer',n=n,size=1,signed=F)
    close(f)
    y
  }
  train <<- load_image_file('iml/mnist/train-images-idx3-ubyte')
  test <<- load_image_file('iml/mnist/t10k-images-idx3-ubyte')
  
  train$y <<- load_label_file('iml/mnist/train-labels-idx1-ubyte')
  test$y <<- load_label_file('iml/mnist/t10k-labels-idx1-ubyte')  
}


show_digit <- function(arr784, col=gray(12:1/12), ...) {
  image(matrix(arr784, nrow=28)[,28:1], col=col, ...)
}


# 3a

cluster <- function(input, k, feature_means){
  p <- ncol(input)
  n <- nrow(input)

  # assign a random cluster for each observation
  cluster <- c(rep(1:k, length.out=n))
  input <- cbind(input, cluster)
  data <- data.frame(input)
  new <- TRUE
  
  if (is.null(feature_means)){
    feature_means <- aggregate(.~cluster, data=data[1:10,], mean)
  }
  
  # iterate until there are no new assigments
  while (new == TRUE){
    new <- FALSE

    # calculate euclidian distance between each observation and the k 
    # assign each observation to the cluster that's closest to it
    for (i in 1:n){
      d <- dist(rbind(input[i,1:p], feature_means[1:k,2:(p+1)]))
      distances <- as.matrix(d)
      dist_from_i <- distances[,1]
      
      new_cluster <- as.integer(which.min(dist_from_i[2:(k+1)]))
      if (new_cluster != data[i,(p+1)]){
        new <- TRUE
        data[i,(p+1)] <- new_cluster
      } 
    }
    
    # calculate feature means (= centroid) for each cluster
    feature_means <- aggregate(.~cluster, data=data, mean)
  }
  
  return(rbind(data, feature_means))
}

library(mvtnorm)

n <- 1000
p <- 2
sigma <- diag(1, nrow=p)
inp <- rmvnorm(1000, c(rep(0, p)), sigma)

k <- 4
clu <- cluster(inp, k, feature_means=NULL)
par(mfrow=c(2,1))
plot(inp[,1:2], col=clu[1:n,3], main="own implementation")

# sanity check with r's kmeans
clust <- kmeans(inp, 4)
plot(inp, col=clust$cluster, main="kmeans")

# 3c

load_mnist()
t <- as.matrix(train$x)
t <- t[1:500,1:784]

cl <- cluster(input=t, k=10, feature_means=NULL)

# prototypes of the clusters
par(mfrow=c(3,4))
for(i in 1:10){
  one <- cl[(500+i),1:784]
  onem <- matrix(unlist(one), ncol=28)
  show_digit(onem, main=cl[(500+i), 785])
}

# some examples belonging to some clusters
par(mfrow=c(5,5))
for(i in 1:25){
  one <- cl[(20+i),1:784]
  onem <- matrix(unlist(one), ncol=28)
  show_digit(onem, main=cl[(20+i), 785])
}

# 3d
# get initial feature means from the first instance of each class
means <- matrix(c(train$x[4,], train$x[6,], train$x[8,], train$x[3,], train$x[1,], train$x[14,], train$x[16,], train$x[18,], train$x[5,], train$x[2,]), nrow=10)
means <- cbind(c(1:10), means)
feature_means <- means
input <- t
k <- 10 

cluster_with_means <- cluster(input=t, k=10, feature_means=means)

# prototypes of the clusters
par(mfrow=c(3,4))
for(i in 1:7){
  one <- cluster_with_means[(500+i),1:784]
  onem <- matrix(unlist(one), ncol=28)
  show_digit(onem, main=cluster_with_means[(500+i), 785])
}