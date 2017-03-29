# Zadanie 1
f <- function(x){
	if (x %/% 1000 %% 2 == 0 && x %% 4 == 0) 1 else
	if (x %/% 1000 %% 2 == 1 && x %% 4 != 0) 1 else 0
}

n <- 1000
N <- 10000

u <- sample(1:N, n, replace=F)
u <- sort(u)
#test <- sort(sample(1:N, n, replace=F))
test <- 1:N

getNeighbours <- function(n, set, elem){
  nearest <- findInterval(elem, set);
  pocz <- nearest;
  
  if (pocz == 0)
    pocz <- 1;
  
  if (pocz < length(set) && abs(set[pocz] - elem) > abs(set[pocz+1] - elem)) {
    pocz <- pocz + 1;
  }
  
  kon <- pocz;
  
  while (kon - pocz + 1 < n) {
    if (pocz > 1 && kon < length(set)) {
      if (abs(set[pocz - 1] - elem) < abs(set[kon + 1] - elem)) {
        pocz <- pocz-1;
      } else {
        kon <- kon+1;
      }
    } else if (kon < length(set)) {
      kon <- kon+1;
    } else {
      pocz <- pocz-1;
    }
  }
  
  set[pocz:kon]
}

knn <- function(n, set, elem){
  neighbours <- getNeighbours(n, set, elem);
	res <- sum(sapply(neighbours, f));
	
	if (res*2 > n)
		1
	else
		0
}

generror <- 0;
for (i in seq(from=1,to=29,by=2)) {
  err <- 0;
  for (e in test) {
	  if (knn(i, u, e) != f(e))
			err <- err+1;
	};
	print(sprintf("%d neighbours -> %.2f%% error", i, err/length(test)*100));
  
  if (i == 1)
    generror <- err/length(test)*100;
}

# Zadanie 2
# Z nierówności Hoeffdinga:
# Szukamy rozwiązania 1 - e^(-2*1000*x^2) = 0.95
# Wynik wynosi 3,87%
errs <- c()
iters <- 10000

for (i in 1:iters) {
  test <- sort(sample(1:N, n, replace=F));
  err <- 0;
  for (e in test) {
    if (knn(1, u, e) != f(e))
      err <- err+1;
  };
  errs <- c(errs, err/length(test)*100);
}

errs <- sort(errs)
print(sprintf("Epsilon = %.2f", errs[iters/100 * 95 + 1] - generror))
# "Epsilon = 2.41"

# Zadanie 3
errs <- c()
iters <- 10000
cross <- 10 # Ile części przy crosswalidacji

for (i in 1:iters) {
  test <- sort(sample(1:N, n, replace=F));
  indices <- sample(rep(1:cross, times = rep(n/cross, times = cross)))
  err <- 0;
  
  for (v in 1:cross) {
    learn <- test[which(indices != v)];
    validate <- test[which(indices == v)];
    
    for (e in validate) {
      if (knn(1, learn, e) != f(e))
        err <- err+1;
    }
  }
  
  errs <- c(errs, err/length(test)*100);
}

errs <- sort(errs)
print(sprintf("Epsilon = %.2f", errs[iters/100 * 95 + 1] - generror))
# "Epsilon = 6.61"