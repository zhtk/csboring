d = 1000
n = 5

#estymator 1: mean(proba)*2
est1 <- c()
#estymator 2: max(proba)*(n+1)/n
est2 <- c()

x <- 1
while(x < 100) {
	x <- x + 1
	proba <- floor(runif(n, min=0, max=d))
	
	est1 <- c(est1, mean(proba)*2)
	est2 <- c(est2, max(proba)*(n+1)/n)
}

print("Wariancja średniej: ")
print(var(est1))

print("Wariancja max: ")
print(var(est2))
