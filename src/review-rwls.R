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
#glm(y ~ x1, family = "binomial")

# RWLS as Newton-Raphson for GLM (logistic regression here)

logReg <- function(X, y, maxIter=80, tolerance=0.01){
	pr <- function(X, B){
		1 / (1 + exp(-X  %*% B))
	}
	##
	weights <- function(X, B, y){
		diag(as.vector(pr(X, B)))
	}
	##
	oldB <- matrix(c(Inf,Inf))
	newB <- matrix(c(0, 0))
	nIter <- 0
	while (colSums((newB - oldB)^2) > tolerance &&
	       nIter < maxIter) {
		oldB <- newB
	## N-R as RWLS
		W <- weights(X, oldB, y)
		hessian <- - t(X) %*% W %*% X
		z <- X %*% oldB + solve(W) %*% (y - pr(X, oldB))
		newB <- solve(-hessian) %*% crossprod(X, W %*% z)
	##
		nIter <- nIter + 1
	}
	newB
}

print(logReg(X, y, tolerance=1E-6, maxIter=100))
