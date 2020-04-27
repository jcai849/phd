suppressMessages(library(pbdDMAT))
init.grid()

set.seed(1234)
# Generate the data

n <- 1000
B <- ddmatrix(c(1,3))
x0 <- rep(1, n)
x1 <- rnorm(n, 0, 1)
X <- as.ddmatrix(cbind(x0, x1))
p <- 1 / (1 + exp(- X %*% B))
y <- ddmatrix(rbinom(n, 1, as.vector(p)))

# Base comparison
#glm(y ~ x1, family = "binomial")

# RWLS as Newton-Raphson for GLM (logistic regression here)

logReg <- function(X, y, maxIter=80, tolerance=0.01){
	pr <- function(X, B){
		1 / (1 + exp(-X  %*% B))
	}
	##
	weights <- function(X, B, y){
		diag(as.vector(pr(X, B)), type="ddmatrix")
	}
	##
	oldB <- ddmatrix(c(Inf,Inf))
	newB <- ddmatrix(c(0, 0))
	nIter <- ddmatrix(0)
	maxIter <- as.ddmatrix(maxIter)
	while (as.matrix(colSums((newB - oldB)^2) > tolerance &
	       nIter < maxIter)) {
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

finalize()
