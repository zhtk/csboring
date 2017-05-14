library("kohonen")

# Zadanie 1
data = rnorm(2,mean=c(0.1,0.1),sd=c(0.5,0.5))
for (i in 2:1000) {
  data = rbind(data, rnorm(2,mean=c(0.1,0.1),sd=c(0.5,0.5)))
}

grid = somgrid(xdim = 1, ydim=1000, topo="rectangular")
somnet = som(data, grid=grid, rlen=1000, alpha=c(0.05,0.01))

x = somnet$codes[[1]][,1]
y = somnet$codes[[1]][,2]
plot(x,y,type="l")

# Zadanie 2
iris.sc = scale(iris[, 1:4])
iris.grid = somgrid(xdim = 10, ydim=10, topo="rectangular")
iris.som = som(iris.sc, grid=iris.grid, rlen=150, alpha=c(0.05,0.01))

groups = 3
iris.hc = cutree(hclust(dist(iris.som$codes[[1]])), groups)
plot(iris.som, type="codes", bgcol=rainbow(groups)[iris.hc])
add.cluster.boundaries(iris.som, iris.hc)
