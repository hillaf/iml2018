# IML week 3

# 1 a

x1 <- 1
x2 <- 2
var_plus <- 16
var_neg <- 1

# Bayes formula
px <- (1/(sqrt(2*pi*var_plus)))*exp(-((x1)^2)/(2*var_plus))*(1/(sqrt(2*pi*var_plus)))*exp(-((x2)^2)/(2*var_plus))
pxneg <- (1/(sqrt(2*pi*var_neg)))*exp(-((x1)^2)/(2*var_neg))*(1/(sqrt(2*pi*var_neg)))*exp(-((x2)^2)/(2*var_neg))
print((px/2)/((pxneg/2)+(px/2)))


# 1 b
x <- as.matrix(expand.grid(.25*(-40:40),.25*(-40:40)))

# P(Y=+1 | x)
sigma_plus <- matrix(c(16,0,0,16), nrow=2)
sigma_neg <- matrix(c(1,0,0,1), nrow=2)

# use dmvnorm to get normal density instead of the ugly formula
p_plus <- dmvnorm(x, c(0,0), sigma_plus)
p_neg <- dmvnorm(x, c(0,0), sigma_neg)

p <- (p_plus/2)/((p_neg/2)+(p_plus/2))
p <- matrix(p, nrow=81)

persp(x=.25*(-40:40),y=.25*(-40:40), p)


