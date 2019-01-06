# IML week 4

# 1 a

# p represents the proportion of training observations in the ith region
# that are from the j:th class
p <- matrix(c(1:6, 1:6), ncol=2)

split_gini <- vector()
split_entropys <- vector()
gini_gains <- vector()
entropy_gains <- vector()
gini_currents <- vector()
entropy_currents <- vector()
weighted_avgs_gini <- vector()
weighted_avgs_entropy <- vector()


gini <- function(p1, p2){
  return(p1*(1-p1)+p2*(1-p2))
}

entropy <- function(p1, p2){
  e <- -(p1*log(p1)+(p2*log(p2)))
  if (is.nan(e)){
    return(0)
  } else {
    return(e)
  }
}

shots <- matrix(c(3, 49,
                  0, 21,
                  6, 21,
                  10, 16,
                  3, 12,
                  16, 11), ncol=2, byrow=TRUE)


for (i in 1:nrow(shots)){
  # class=1 for saved shots
  p[i, 1] <- shots[i,1]/(shots[i,1]+shots[i,2])
  # class=2 for scored shots
  p[i, 2] <- ((shots[i,1]+shots[i,2])-shots[i,1])/(shots[i,1]+shots[i,2])
}

for (i in 1:nrow(shots)){
  # calculate gini and entropy for the current un-splitted area
  p1_current <- sum(shots[i:nrow(shots),1])/sum(shots[i:nrow(shots),1]+shots[i:nrow(shots),2])
  p2_current <- sum(shots[i:nrow(shots),2])/sum(shots[i:nrow(shots),1]+shots[i:nrow(shots),2])
  gini_currents <- c(gini_currents, gini(p1_current, p2_current))
  entropy_currents <- c(entropy_currents, entropy(p1_current, p2_current))


  # calculate average impurity and weighted gain for the split
  if (i < nrow(shots)){
    # calculate gini and entropy for the area to be splitted
    split_gini <- c(split_gini, gini(p[i,1], p[i,2]))
    split_entropys <- c(split_entropys, entropy(p[i,1], p[i,2]))
    
    # impurities for the remaining area
    p1_remaining <- sum(shots[(i+1):nrow(shots),1])/sum(shots[(i+1):nrow(shots),1]+shots[(i+1):nrow(shots),2])
    p2_remaining <- sum(shots[(i+1):nrow(shots),2])/sum(shots[(i+1):nrow(shots),1]+shots[(i+1):nrow(shots),2])
    remaining_gini <- gini(p1_remaining, p2_remaining)
    remaining_entropy <- entropy(p1_remaining, p2_remaining)
    
    # calculate weights
    weight_remaining <- sum(shots[(i+1):nrow(shots),1]+shots[(i+1):nrow(shots),2])
    weight_split <- sum(shots[i,1]+shots[i,2])
    
    # weighted averages for the split
    weighted_avgs_gini <- c(weighted_avgs_gini, (weight_split/(weight_split+weight_remaining))*gini(p[i,1], p[i,2])+(weight_remaining/(weight_split+weight_remaining))*remaining_gini)
    weighted_avgs_entropy <- c(weighted_avgs_entropy, (weight_split/(weight_split+weight_remaining))*entropy(p[i,1], p[i,2])+(weight_remaining/(weight_split+weight_remaining))*remaining_entropy)
    
    # gains
    gini_gains[i] <- gini_currents[i]-weighted_avgs_gini[i]
    entropy_gains[i] <- entropy_currents[i]-weighted_avgs_entropy[i]
  }

  # print("---")
  # print("Current (without the split): ")
  # cat("Gini: ", gini_currents[i], ", entropy: ", entropy_currents[i])
  # print("")
  # cat("For split R", i, " : Gini ", split_gini[i], ", entropy: ", split_entropys[i])
  # print("and")
  # # print remaining area
  # print("Gains: ")
  # cat("Gini: ", gini_gains[i], " entropy: ", entropy_gains[i])
  # print("---")
}


plot(gini_gains, type="l", ylim=0:1/5, ylab="Gain in split")
lines(entropy_gains, type="l", lty=2)