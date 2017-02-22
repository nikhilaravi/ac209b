library(Brobdingnag)
posterior_pA = function(alpha, yA = NULL, yB = NULL, y_til = NULL){
  # number of features
  K = length(yA)
  # total word counts
  n = sum(y_til)
  nA = sum(yA)
  nB = sum(yB)
  # posterior predictive distribution of being class A
  A1 = lfactorial(n) + lfactorial(nA) - lfactorial(n + nA)
  A2 = sum(lfactorial(y_til + yA)) - sum(lfactorial(y_til)) - sum(lfactorial(yA))
  A3 = lfactorial(n + nA) + lgamma(K*alpha) - lgamma(n + nA + K*alpha)
  A4 = sum(lgamma(y_til + yA + alpha) - lfactorial(y_til + yA) - lgamma(alpha))
  A5 = lfactorial(nB) + lgamma(K*alpha) - lgamma(nB + K*alpha)
  A6 = sum(lgamma(yB + alpha) - lfactorial(yB) - lgamma(alpha))
  R_A = exp(as.brob(A1+A2+A3+A4+A5+A6))
  # posterior predictive distribution of being class B
  B1 = lfactorial(n) + lfactorial(nB) - lfactorial(n + nB)
  B2 = sum(lfactorial(y_til + yB)) - sum(lfactorial(y_til)) - sum(lfactorial(yB))
  B3 = lfactorial(n + nB) + lgamma(K*alpha) - lgamma(n + nB + K*alpha)
  B4 = sum(lgamma(y_til + yB + alpha) - lfactorial(y_til + yB) - lgamma(alpha))
  B5 = lfactorial(nA) + lgamma(K*alpha) - lgamma(nA + K*alpha)
  B6 = sum(lgamma(yA + alpha) - lfactorial(yA) - lgamma(alpha))
  R_B = (exp(as.brob(B1 + B2 + B3 + B4 + B5 + B6)))
  # probability of being class A
  pA = as.numeric(R_A/(R_A + R_B))
  return(pA)
}


## posterior_pA above generates probabilities for a single case.
## here we wrap it so you can calculate probabilities for a data.frame of cases.

class_probability = function(data, class, alpha) {
  y <- lapply(split(data, class),
              function(x) {
                apply(x, 2, sum)
              })
  yA <- y[[1]]
  yB <- y[[2]]
  pA <- apply(data, 1, function(x) {
    posterior_pA(alpha = alpha,
                 yA = yA,
                 yB = yB,
                 y_til = x)
  })
  pA
}

pa1 <- class_probability(train_1[-1], train_1[1], 1) 

## confusion matrix, as proportions.
confmat(train_1$V1,
        factor(pa1 < .5,
               labels = c("AaronPressman", "AlanCrosby")))