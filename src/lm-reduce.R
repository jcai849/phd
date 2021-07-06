library(biglm)

nc <- 15
cs <- split(iris, cumsum((seq(nrow(iris)) - 1) %% nc == 0)) # split to chunks
model <- Reduce(f = update, x = cs[-1],
		init = biglm(Petal.Length ~ Sepal.Width * Sepal.Length, cs[[1]]))
