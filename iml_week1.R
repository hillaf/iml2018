# IML week 1

# 1b

simu <- function(n, p){
  eps <- sqrt(log(0.05/2)/(-2*n))
  sim <- rbinom(10000, n, p)
  sim <- sim[sim > n*(p-eps)]
  sim <- sim[sim < n*(p+eps)]
  return(length(sim))
}

print("n = 10")
print(simu(10, 0.5))
print(simu(10, 0.9))
print(simu(10, 0.99))

print("n = 100")
print(simu(100, 0.5))
print(simu(100, 0.9))
print(simu(100, 0.99))

print("n = 1000")
print(simu(1000, 0.5))
print(simu(1000, 0.9))
print(simu(1000, 0.99))



# 1c

interval <- function(n, p, k){
  eps <- sqrt(log(0.05/(2*k))/(-2*n))
  print("interval for k = ")
  print(k)
  print("from: ")
  print(n*(p-eps))
  print("to: ")
  n*(p+eps)
}

print("---- 1C: ----")
print("n = 10")
print(interval(10, 0.5, 1))
print(interval(10, 0.5, 10))
print(interval(10, 0.5, 100))

print("n = 100")
print(interval(100, 0.5, 1))
print(interval(100, 0.9, 10))
print(interval(100, 0.99, 100))

print("n = 1000")
print(interval(1000, 0.5, 1))
print(interval(1000, 0.9, 10))
print(interval(1000, 0.99, 100))


# 1 d
simul <- function(n, p){
  eps <- sqrt(log(0.05/2)/(-2*n))
  sim <- rbinom(100, n, p)
  sim <- sim[sim > n*(p-eps)]
  sim <- sim[sim < n*(p+eps)]
  return(length(sim))
}


sum <- 0;

for(i in 1:10000){
  arr <- simul(1000, 0.9)
  if (arr == 100){
    sum <- sum+1;
  }
}
print(sum)


# 2 a
college <- read.csv("College.csv")

# b
college = college [ , -1]
fix(college)

# c
summary(college)
pairs(college[,1:10])
plot(college[,1], college[,9], xlab="Private", ylab="Outstate")

Elite = rep ("No", nrow (college))
Elite [college$Top10perc > 50]="Yes"
Elite = as.factor (Elite)
college = data.frame (college,Elite )

summary(college)
plot(college[,19], college[,9], xlab="Elite", ylab="Outstate")

op <- par(mfrow = c(2, 2))
hist(college[,2], main = paste("Applications received"), xlab="Apps")
hist(college[,4], main = paste("New students from top 10 % of high school class"), xlab="Top 10%")
hist(college[,12], main = paste("Estimated personal spending"), xlab="Personal spending")
hist(college[,18], main = paste("Graduation rate"), xlab="Graduation rate")

