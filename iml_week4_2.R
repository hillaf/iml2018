# IML week 4

# 2
library(nnet)

create_data <- function(n){
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

# smoothed class distribution
smoothed_class <- function(smoothing, y){
  n <- length(y)
  nc <- vector()
  for(i in 0:2){
    nc[i+1] <- length(y[y == i])
  }
  
  # sc_0 <- nc/n
  # sc_12 <- (nc+1/2)/(n+3*(1/2))
  # sc_1 <- (nc+1)/(n+3)
  
  return((nc+smoothing)/(n+smoothing))
}


classify <- function(x, smc, smoothed_x_0, smoothed_x_1, smoothed_x_2){
  predictions <- vector()
  
  # classify P(Y=c | x) using Bayes
  # Laplace smoothed priori prob for y=c is in sc_1[c+1]
  # Laplace smoothed cond probability for P(Xj=xi | Y=c)
  # is in smoothed_x_c[j,xi]
  
  for(i in 1:nrow(x)){
    sample1 <- x[i,1]+1
    sample2 <- x[i,2]+1
    
    # naive Bayes assumption
    p0 <- smoothed_x_0[1,sample1]*smoothed_x_0[2,sample2]*smc[1]
    p1 <- smoothed_x_1[1,sample1]*smoothed_x_1[2,sample2]*smc[2]
    p2 <- smoothed_x_2[1,sample1]*smoothed_x_2[2,sample2]*smc[3]
    
    if(max(p0, p1, p2) == p0){
      predictions[i] <- 0
    } else if (max(p0, p1, p2) == p1){
      predictions[i] <- 1
    } else if (max(p0, p1, p2) == p2){
      predictions[i] <- 2
    }
  }

  return(predictions)
}

true_classify <- function(x){
  predictions <- vector()
  
  for(i in 1:nrow(x)){
    sample1 <- x[i,1]+1
    sample2 <- x[i,2]+1
    
    x0 <- matrix(c(0.2, 0.4, 0.0, 0.1, 0.2, 0.1), ncol=2)
    x1 <- matrix(c(0.6, 0.1, 0.1, 0.1, 0.1, 0.0), ncol=2)
    x2 <- matrix(c(0.1, 0.3, 0.2, 0.4, 0.0, 0.0), ncol=2)
    y <- c(0.4, 0.3, 0.3)
    
    p0 <- x0[sample2, sample1]*y[1]
    p1 <- x1[sample2, sample1]*y[2]
    p2 <- x2[sample2, sample1]*y[3]
    
    if(max(p0, p1, p2) == p0){
      predictions[i] <- 0
    } else if (max(p0, p1, p2) == p1){
      predictions[i] <- 1
    } else if (max(p0, p1, p2) == p2){
      predictions[i] <- 2
    }
  }
  
  return(predictions)
}


calculate_log_loss <- function(td, smc, smoothed_x_0, smoothed_x_1, smoothed_x_2){
  ll <- vector()
  x <- td[,1:2]
  results <- td[,3]
  
  for (i in 1:nrow(x)){
    sample1 <- x[i,1]+1
    sample2 <- x[i,2]+1
    class <- results[i]
    
    # naive Bayes assumption
    p0 <- smoothed_x_0[1,sample1]*smoothed_x_0[2,sample2]*smc[1]
    p1 <- smoothed_x_1[1,sample1]*smoothed_x_1[2,sample2]*smc[2]
    p2 <- smoothed_x_2[1,sample1]*smoothed_x_2[2,sample2]*smc[3]
    nominator <- p0+p1+p2
    
    # dodge zero value problems caused by small training data set
    if (p0 == 0){
      p0 <- 0.0000001
    } else if (p1 == 0){
      p1 <- 0.0000001
    } else if (p2 == 0){
      p2 <- 0.0000001
    } else if (nominator == 0){
      nominator <- 0.0000001
    }
    
    p0 <- p0/nominator
    p1 <- p1/nominator
    p2 <- p2/nominator

      if (class == 0){
        ll[i] <- -(log2(p0))
      } else if (class == 1){
        ll[i] <- -(log2(p1))
      } else if (class == 2){
        ll[i] <- -(log2(p2))
      }
  }
  return(sum(ll, na.rm=TRUE))
}


calculate_error <- function(pre, td){
  error <- vector()
  x <- td[,1:2]
  results <- td[,3]
  match <- pre == results
  
  return(length(match[match == FALSE]))
}


logres_logloss <- function(td, probabilities){
  ll <- vector()
  results <- td[,3]
  
  for (i in 1:nrow(td)){
    p <- probabilities[i,results[i]+1]
    ll[i] <- -log2(p)
  }
  return(sum(ll, na.rm=TRUE))
}


calculate_bayes_error <- function(){
  y <- c(0.4, 0.3, 0.3)
  x0 <- matrix(c(0.2, 0.4, 0.0, 0.1, 0.2, 0.1), ncol=2)*y[1]
  x1 <- matrix(c(0.6, 0.1, 0.1, 0.1, 0.1, 0.0), ncol=2)*y[2]
  x2 <- matrix(c(0.1, 0.3, 0.2, 0.4, 0.0, 0.0), ncol=2)*y[3]
  prob <- 0

    for (i in 1:2){
      for (j in 1:3){
        p0 <- x0[j,i]
        p1 <- x1[j,i]
        p2 <- x2[j,i]
        
        if(max(p0, p1, p2) == p0){
          prob <- prob + p1 + p2
        } else if (max(p0, p1, p2) == p1){
          prob <- prob + p0 + p2
        } else if (max(p0, p1, p2) == p2){
          prob <- prob + p0 + p1
        }
      }
    }
  
  return(prob)
}



naive_bayes_log_losses <- vector()
logres_log_losses <- vector() 
interaction_log_losses <- vector()

naive_bayes_error_rates <- vector()
logres_error_rates <- vector()
bayes_error_rates <- vector()
interaction_error_rates <- vector()

a <- c(25, 50, 100, 200, 400, 800, 1600, 3200, 6400)
smoothings <- c(0, 1/3, 1/2, 1)
smoothing <- 0
n <- 10000
test_data <- create_data(n)

for(i in a){
  training_data <- create_data(i)
  
  # 2a log losses for naive bayes and logistic regression
  # naive Bayes
  # obtain smoothed conditional probability distributions for each class
  smoothed_c <- smoothed_class(smoothing, training_data[,3])
  smx0 <- smoothed(training_data[,1:2], training_data[,3], c=0, smoothing)
  smx1 <- smoothed(training_data[,1:2], training_data[,3], c=1, smoothing)
  smx2 <- smoothed(training_data[,1:2], training_data[,3], c=2, smoothing)
  predictions <- classify(test_data[,1:2], smoothed_c, smx0, smx1, smx2)

  logloss <- calculate_log_loss(test_data, smoothed_c, smx0, smx1, smx2)
  naive_bayes_log_losses <- c(naive_bayes_log_losses, logloss)
  
  error_rate <- calculate_error(predictions, test_data) 
  naive_bayes_error_rates <- c(naive_bayes_error_rates, error_rate)
 
   
  # logistic regression
  D <- data.frame(Y = training_data[,3], X1 = factor(training_data[ ,1]), X2 = factor(training_data[ ,2]))
  model <- multinom(Y~X1+X2, data=D)
  nd <- data.frame(X1=factor(test_data[,1]), X2=factor(test_data[,2]))
  pr <- predict(model, nd)
  prob <- predict(model, type="probs", newdata=nd)
  
  logres_log_losses <- c(logres_log_losses, logres_logloss(test_data, prob))
  logres_error_rates <- c(logres_error_rates, calculate_error(pr, test_data))
  
  
  # 2b Bayes classifier
  bayes <- true_classify(test_data[,1:2])
  bayes_error_rates <- c(bayes_error_rates, calculate_error(bayes, test_data))
  
  # 2c Bayes error
  bayes_error <- calculate_bayes_error()
  bayes_vector <- rep(bayes_error, 9)*n
  
  # 2d logistic regression with interaction
  D <- data.frame(Y = training_data[,3], X1 = factor(training_data[ ,1]), X2 = factor(training_data[ ,2]))
  model <- multinom(Y~X1*X2, data=D)
  nd <- data.frame(X1=factor(test_data[,1]), X2=factor(test_data[,2]))
  pr <- predict(model, nd)
  prob <- predict(model, type="probs", newdata=nd)
  
  interaction_log_losses <- c(interaction_log_losses, logres_logloss(test_data, prob))
  interaction_error_rates <- c(interaction_error_rates, calculate_error(pr, test_data))
}


plot(1:9, naive_bayes_log_losses, type="l", ylim=c(1300, 40000), ylab="Log loss", axes=FALSE)
axis(1, at = 1:9, labels = a)
axis(2)
lines(1:9, logres_log_losses, type="l", col="red")
lines(1:9, interaction_log_losses, type="l", col="blue")

plot(x=1:9, naive_bayes_error_rates, type="l", ylim=c(2000,8000), ylab="Error rates", axes=FALSE)
axis(1, at = 1:9, labels = a)
axis(2)
lines(1:9, logres_error_rates, type="l", col="red")
lines(1:9, bayes_error_rates, type="l", col="green")
lines(1:9, bayes_vector, type="l", lty=2)
lines(1:9, interaction_error_rates, type="l", col="blue")