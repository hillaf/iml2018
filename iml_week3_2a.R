# IML week 3

# 2 a

create_training_data <- function(n){
  allx <- NA
  ally <- vector()
  
  for (i in 1:n){
    y <- sample(0:2, size=1, replace=TRUE, prob=c(0.4,0.3,0.3))
    if (y == 0){
      x <- expand.grid(0:1,0:2)[sample(1:6, 1, replace=TRUE, prob=c(0.2,0.1,0.4,0.2,0.0,0.1)),]
    } else if (y == 1){
      x <- expand.grid(0:1,0:2)[sample(1:6, 1, replace=TRUE, prob=c(0.6,0.1,0.1,0.1,0.1,0.0)),]
    } else if (y == 2){
      x <- expand.grid(0:1,0:2)[sample(1:6, 1, replace=TRUE, prob=c(0.1,0.4,0.3,0.0,0.2,0.0)),]
    }
    
    ally <- c(ally, y)
    allx <- rbind(allx, as.matrix(x))
  }
  
  allx <- allx[1:n+1,1:2]
  return(cbind(allx, ally))
}

n <- 100
all <- create_training_data(n)
ally <- all[,3]
allx <- all[,1:2]

avgy <- length(ally[ally == 0])
avgx <- 0

for (i in 1:length(ally)){
  if (allx[i,1] == 0 & allx[i,2] == 0){
    avgx = avgx + 1
  }
}

# there are rouhgly 29/100 cases of X = (0,0) as (0,4*0,2) + (0,3*0,6) + (0,3*0,1) = 0,29
print(avgy)
print(avgx)


# 2b




smoothed <- function(allx, y, c, smoothing){
  mcjx <- smoothing
  nc <- length(y[y == c])
  ncjx <- matrix(0, nrow=2, ncol=3)
  mcj <- 6*smoothing
  
  for (j in 1:2){
    for (x in 0:2){
      for (i in 1:length(y)){
        if (y[i] == c & allx[i,j] == x){
          ncjx[j,x+1] <- ncjx[j,x+1] + 1
        }
      }
    }
  }
  
  # estimate P(Xj = x | Y = c)
  smoothed <- (ncjx + mcjx)/(nc+mcj)
  return(smoothed)
}

print("-- Maximum likelihood: --")
print(smoothed(allx, ally, c=0, smoothing=0))
print(smoothed(allx, ally, c=1, smoothing=0))
print(smoothed(allx, ally, c=2, smoothing=0))

print("-- Laplace: --")
print(smoothed(allx, ally, c=0, smoothing=1))
print(smoothed(allx, ally, c=1, smoothing=1))
print(smoothed(allx, ally, c=2, smoothing=1))

print("-- Krichevsky-Trofimov: --")
print(smoothed(allx, ally, c=0, smoothing=1/2))
print(smoothed(allx, ally, c=1, smoothing=1/2))
print(smoothed(allx, ally, c=2, smoothing=1/2))

print("-- Jääsaari: --")
print(smoothed(allx, ally, c=0, smoothing=1/3))
print(smoothed(allx, ally, c=1, smoothing=1/3))
print(smoothed(allx, ally, c=2, smoothing=1/3))


# smoothed class distribution
  
nc <- vector()
for(i in 0:2){
  nc[i+1] <- length(ally[ally == i])
}

sc_0 <- nc/n
sc_12 <- (nc+1/2)/(n+3*(1/2))
sc_1 <- (nc+1)/(n+3)

# obtain Laplace smoothed conditional probability distributions for next exercise
smoothed_x_0 <- smoothed(allx, ally, c=0, smoothing=1)
smoothed_x_1 <- smoothed(allx, ally, c=1, smoothing=1)
smoothed_x_2 <- smoothed(allx, ally, c=2, smoothing=1)

# 2 c

test_n <- 10000
test_set <- create_training_data(test_n)
predictions <- vector()
x <- test_set[,1:2]
results <- test_set[,3]

# classify P(Y=c | x) using Bayes
# Laplace smoothed priori prob for y=c is in sc_1[c+1]
# Laplace smoothed cond probability for P(Xj=xi | Y=c)
# is in smoothed_x_c[j,xi]

for(i in 1:nrow(x)){
  sample1 <- x[i,1]+1
  sample2 <- x[i,2]+1

  # naive Bayes assumption
  p0 <- smoothed_x_0[1,sample1]*smoothed_x_0[2,sample2]*sc_1[1]
  p1 <- smoothed_x_1[1,sample1]*smoothed_x_1[2,sample2]*sc_1[2]
  p2 <- smoothed_x_2[1,sample1]*smoothed_x_2[2,sample2]*sc_1[3]
  
  if(max(p0, p1, p2) == p0){
    predictions[i] <- 0
  } else if (max(p0, p1, p2) == p1){
    predictions[i] <- 1
  } else {
    predictions[i] <- 2
  }
}

# calculate error rate

match <- predictions == results
print("error rate:")
print(length(match[match == FALSE])/test_n)
