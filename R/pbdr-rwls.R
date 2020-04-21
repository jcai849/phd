set.seed(1234)
library(pbdDMAT, quiet=TRUE)
init.grid()

bldim <- c(4,4)

# Generate the data
if (comm.rank() == 0) {
	n <- 1000
	B <- matrix(c(1,3))
	x0 <- rep(1, n)
	x1 <- rnorm(n, 0, 1)
	X <- cbind(x0, x1)
	p <- 1 / (1 + exp(- X %*% B))
	y <- rbinom(n, 1, p)
} else {
	X <- NULL
	y <- NULL
}

dX <- as.ddmatrix(x=X, bldim=bldim)
dy <- as.ddmatrix(x=y, bldim=bldim)

# Base comparison
#glm(y ~ x1, family = "binomial")

# RWLS as Newton-Raphson for GLM (logistic regression here)

logReg <- function(X, y, maxIter=80, tolerance=0.01){
	pr <- function(X, B){
		1 / (1 + exp(-X  %*% B))
	}
	##
	weights <- function(X, B, y){
		W <- ddmatrix(0, N, N, bldim=bldim)
		diag(W) <- pr(X, B)
		W
	}
	##
	N <- nrow(X)
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

dHatB <- logReg(dX, dy, tolerance=1E-6, maxIter=100)
HatB <- as.matrix(dHatB, proc.dest=0)
comm.print(HatB, rank.print=0)

finalize()
