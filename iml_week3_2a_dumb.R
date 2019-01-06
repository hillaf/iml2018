# IML week3 
# 2 a

avgv <- 0
avgv2 <- 0

for (i in 1:10000){
  v <- sample(0:2, size=1, replace=TRUE, prob=c(0.4,0.3,0.3))
  if (v == 0){
    v2 <- expand.grid(0:1,0:2)[sample(1:6, 1, replace=TRUE, prob=c(0.2,0.1,0.4,0.2,0.0,0.1)),]
  } else if (v == 1){
    v2 <- expand.grid(0:1,0:2)[sample(1:6, 1, replace=TRUE, prob=c(0.6,0.1,0.1,0.1,0.1,0.0)),]
  } else if (v == 2){
    v2 <- expand.grid(0:1,0:2)[sample(1:6, 1, replace=TRUE, prob=c(0.1,0.4,0.3,0.0,0.2,0.0)),]
  }
  
  avgv <- avgv + length(v[v == 0])
  avgv2 <- avgv2 + nrow(v2[v2$Var1 == 0 & v2$Var2 == 0,])
}

# there are rouhgly 29/100 cases of X = (0,0) as (0,4*0,2) + (0,3*0,6) + (0,3*0,1) = 0,29
print(avgv/100)
print(avgv2/100)