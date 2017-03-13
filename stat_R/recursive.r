# Zadanie 1
fr <- function(x) if (x>0) x*fr(x-1) else 1
fr(5)

# Zadanie 2
f <- function(x, acc = 0) if (x != 0) f(x%/%10, acc*10 + x%%10) else acc

# Zadanie 3
fi <- function(x){
	w <- 1; 
	while (x>0) {
		w<-w*x; 
		x<-x-1
	}; 
	w
}
fi(5)

# Zadanie 4
x <- sin(1:100)
A = -1
B = 0
C = 1
y <- ifelse(x < -0.5, "A", ifelse(x <= 0.5, "B", "C"))

