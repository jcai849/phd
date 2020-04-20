set.seed(1234)
# Generate the data

n <- 1000
B <- matrix(c(1,3))
x0 <- rep(1, n)
x1 <- rnorm(n, 0, 1)
X <- cbind(x0, x1)
p <- 1 / (1 + exp(- X %*% B))
y <- rbinom(n, 1, p)

# Base comparison
glm(y ~ x1, family = "binomial")

# RWLS

pr <- function(X, B){
	1 / (1 + exp(-X  %*% B))
}

score <- function(X, B, y){
	crossprod(X, y - pr(X, B))
}

hessian <- function(X, B, y){
	N <- nrow(X)
	W <- weights(X, B, y)
	- t(X) %*% W %*% X
}

weights <- function(X, B, y){
	W <- matrix(0, N, N)
	diag(W) <- pr(X, B)
	W
}

newton <- function(B, X, y){
	z <- X %*% B + solve(weights(X, B, y)) %*% (y - pr(X, B))
	solve(-hessian(X, B)) %*% crossprod(X, weights(X, B, y) %*% z)
}
