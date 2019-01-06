# IML week 4

# 3 a

# draw data

draw_data <- function(n){
  samples_plus <- matrix(NA, nrow=400, ncol=2)
  samples_minus <- matrix(NA, nrow=400, ncol=2) 
  classes <- vector()
  
  for(i in 1:n){
    if(sample(c(0,1), 1, replace=TRUE, prob=c(0.5, 0.5)) == 1){
      sample <- rnorm(2, mean=0, sd=1)
      samples_minus[i,] <- sample
      class <- -1
    } else {
      sample <- rnorm(2, mean=0, sd=4)
      samples_plus[i,] <- sample
      class <- 1
    }
    classes <- c(classes, class)
  }

  # put vectors together and remove NA's
  samples_plus <- rbind(samples_plus, samples_minus)
  samples_plus <- cbind(samples_plus, classes)
  samples_plus <- samples_plus[!rowSums(!is.finite(samples_plus)),]
  samples_minus <- samples_minus[!rowSums(!is.finite(samples_minus)),]
  return(samples_plus)
}

error_rate <- function(pre, class){
  match <- pre == class
  match <- match[match == FALSE]
  return(length(match)/200)
}


error_rates <- matrix(NA, ncol=6, nrow=100)

for(j in 1:100){
  samples <- draw_data(200)
  newdata <- draw_data(200)
  class <- samples[,3]
  sample <- data.frame(x=samples[,1:2], y=as.factor(class))
  nd <- data.frame(x=newdata[,1:2])
  
  for (i in 1:3){
    kernel <- switch (i,
                      "linear",
                      "polynomial",
                      "radial")
    
    if(kernel=="polynomial"){
      svmfit = svm(y~., data = sample, kernel =kernel , cost = 1 , scale = FALSE, degree=2, gamma=1)
    } else {
      svmfit = svm(y~., data = sample, kernel =kernel , cost = 1, scale = FALSE, gamma=1)
    }
    
    #plot(svmfit, sample, ylim=c(-10, 10), xlim=c(-10,10))
    
    # prediction with non-squared input
    pre <- predict(svmfit, nd)
    error_rates[j,i] <- error_rate(pre, class)
  }
  
  
  # add transformed features to data
  sample <- cbind(samples[,1:2], samples[,1:2]^2)
  sample <- data.frame(x=sample, y=as.factor(class))
  
  newdata <- cbind(newdata[,1:2], newdata[,1:2]^2)
  nd <- data.frame(x=newdata)
  
  for (i in 1:3){
    kernel <- switch (i,
                      "linear",
                      "polynomial",
                      "radial")
    
    if(kernel=="polynomial"){
      svmfit = svm(y~., data = sample, kernel =kernel , cost = 1 , scale = FALSE, degree=2, gamma=1)
    } else {
      svmfit = svm(y~., data = sample, kernel =kernel , cost = 1, scale = FALSE, gamma=1)
    }
    
    # predict with squared features
    # enlarging the feature space with squared features leads to a non-linear decision boundary
    # thus the improvement
    pre <- predict(svmfit, nd)
    error_rates[j,i+3] <- error_rate(pre, class)
  }
}

means <- vector()
for(i in 1:6){
  means[i] <- mean(error_rates[,i])
}

print(means)