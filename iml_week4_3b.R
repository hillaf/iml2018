# IML week 4

# 3 b

data <- ISLR::OJ

library(dplyr)
train<-sample_frac(data, 0.7476)
sid<-as.numeric(rownames(train)) # because rownames() returns character
test<-data[-sid,]
test_error_rates <- vector()
train_error_rates <- vector()


pre <- function(mod, train, test){
  pre <- predict(mod, train)
  train_error <- pre == train$Purchase
  train_error <- length(train_error[train_error == FALSE])/length(train_error)

  pre <- predict(mod, test)
  test_error <- pre == test$Purchase
  test_error <- length(test_error[test_error == FALSE])/length(test_error)
  return(c(train_error, test_error))
}


# linear model
tune.out = tune(svm, Purchase~.,  data = train , kernel ="linear",
                ranges = list(cost = c(0.01 , 0.05, 0.1, 0.5, 1, 5, 10)))
summary(tune.out)
bestmod = tune.out$best.model
summary(bestmod)
errors <- pre(bestmod, train, test)
train_error_rates <- c(train_error_rates, errors[1])
test_error_rates <- c(test_error_rates, errors[2])


# radial kernel
tune.out = tune(svm, Purchase~.,  data = train , kernel ="radial",
                ranges = list(cost = c(0.01 , 0.05, 0.1, 0.5, 1, 5, 10)))
summary(tune.out)
bestmod = tune.out$best.model
summary(bestmod)

errors <- pre(bestmod, train, test)
train_error_rates <- c(train_error_rates, errors[1])
test_error_rates <- c(test_error_rates, errors[2])



# polynomial model
tune.out = tune(svm, Purchase~.,  data = train , kernel ="polynomial",
                ranges = list(cost = c(0.01 , 0.05, 0.1, 0.5, 1, 5, 10)), degree=2)
summary(tune.out)
bestmod = tune.out$best.model
summary(bestmod)

errors <- pre(bestmod, train, test)
train_error_rates <- c(train_error_rates, errors[1])
test_error_rates <- c(test_error_rates, errors[2])

#plot(test_error_rates, type="l", ylim=c(0,1/2), ylab="Error rates")
#lines(train_error_rates, lty=2)

print(train_error_rates)
print(test_error_rates)