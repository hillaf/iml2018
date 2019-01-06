# iml week 2

# 1 a

Sigma <- matrix(c(2.0, -1.837, -1.837, 3.0), nrow=2, ncol=2)
dist <- mvrnorm(200, mu=c(0,0), Sigma)
cov(dist[,1], dist[,1])
cov(dist[,1], dist[,2])
cov(dist[,2], dist[,1])
cov(dist[,2], dist[,2])
cor(dist[,1], dist[,2])

# 1 b

plot(dist[,1], dist[,2])
kde <- kde2d(dist[,1], dist[,2])
contour(kde)
image(kde)
persp(kde)

# 1 c

x <- expand.grid(.25*(-20:20),.25*(-20:20))
xm <- as.matrix(x)

# use dmvnorm to get multivariate normal density
dmv <- dmvnorm(xm, c(0,0), Sigma)
dmvm <- matrix(dmv, nrow=41)

contour(dmvm)
image(dmvm)
persp(dmvm)


# 1 d

Sigma2 <- matrix(c(1.0, 2.837, 2.837, 5.0), nrow=2, ncol=2)

# use dmvnorm to get multivariate normal density
dmv2 <- dmvnorm(xm, c(2,1), Sigma)
dmvm2 <- matrix(dmv2, nrow=41)

# calculate ratio p(Y=1 | x)
p <- (dmvm/2)/((dmvm/2)+(dmvm2/2))
plot(p)
contour(p)
image(p)


# kangaskassikuosit #1

dist2 <- rmvnorm(1681, mean=c(0,0), Sigma)
dist2m <- dmvnorm(dist2, c(0,0), Sigma)

p2 <- (dist2m/2)/((dist2m/2)+(dmv/2))
p2 <- matrix(p2, nrow=41)
contour(p2)



# kangaskassikuosit

# dist2 <- mvrnorm(1681, mu=c(0,0), Sigma)
# 
# dist2mv1 <- dmvnorm(dist2, c(0,0), Sigma)
# dist2mv2 <- dmvnorm(dist2, c(2,1), Sigma)
# 
# p2 <- (dist2mv1/2)/((dist2mv1/2)+(dist2mv2/2))
# p2 <- matrix(p2, nrow=41)
# contour(p2)

