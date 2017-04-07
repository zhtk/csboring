library(e1071)
library(nnet)
library(AMORE)

# Zadanie 1

f <- function(x){
  if (x == 0 || x == 3)
    1
  else
    0
}

data = as.numeric(0:3)
corr = sapply(data, f)

myNNet = newff(n.neurons = c(1,8,1), learning.rate.global = 1.1, momentum.global = 0.8,
               error.criterium = "LMS", Stao=NA, hidden.layer = "sigmoid", output.layer = "sigmoid")

myNNet = train(myNNet, as.data.frame(data), as.numeric(corr), report=F, n.shows = 100, show.step = 5000)
err = max(abs(sim.MLPnet(myNNet$net,data) - corr))
print(err)

# Zadanie 2

ir <- rbind(iris3[,,1],iris3[,,2],iris3[,,3])
targets <- class.ind( c(rep("s", 50), rep("c", 50), rep("v", 50)) )
samp <- c(sample(1:50,25), sample(51:100,25), sample(101:150,25))
ir1 <- nnet(ir[samp,], targets[samp,], size = 2, rang = 0.1,
            decay = 5e-4, maxit = 200)
test <- function(true, pred) {
  true <- max.col(true)
  cres <- max.col(pred)
  table(true, cres)
}
t <- test(targets[-samp,], predict(ir1, ir[-samp,]))
print(t)