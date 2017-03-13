# Zadanie 1
data <- iris[ which(iris$Petal.Width>1.5), ]
print(data$Species)

# Zadanie 2
# install.packages("tree")
library(tree)

n <- 10
m <- dim(iris)[1]

# Podział zbioru testowego
indices <- sample(rep(1:n, times = rep(m/n, times = n)))

fit <- c()
valid <- c()

for (i in 1:n) {
		# Podział na zbiory uczące i weryfikujące
        iris.learn <- iris[which(indices != i),]
        iris.valid <- iris[which(indices == i),]
        
        # Nauka w modelu drzewkowym
        ir.tr <- tree(Species ~., iris.learn)
		
		# Wyniki przewidywania
        fit <- c(fit, predict(ir.tr, iris.valid, type="class"))
        valid <- c(valid, iris.valid$Species)
        
        i <- i + 1
}

# Wypisanie macierzy konfuzji
fit <- factor(fit,labels=c("setosa","versicolor","virginica"))
valid <- factor(valid,labels=c("setosa","versicolor","virginica"))
mat <- table(valid, fit)
print(mat)
