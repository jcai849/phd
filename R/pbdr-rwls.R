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
		ddmatrix(1 / (1 + exp(-X  %*% B)),
			    bldim=bldim)
	}
	## generating a distributed matrix of weights over the cluster is
	## decidedly nontrivial; see section 7 of Guide to the pbdDMAT package,
	## April 2020. Here I'll just generate one on just one node, then
	## broadcast it.
	weights <- function(X, B, y){
		if (comm.rank() == 0){
			W <- matrix(0, N, N)
			diag(W) <- pr(X, B)
		} else {
			W <- NULL
		}
		as.ddmatrix(x=W, bldim=bldim)
	}
	##
	N <- nrow(X)
	if (comm.rank() ==0){
		oldB <- matrix(c(Inf,Inf))
	} else {
		oldB <- NULL
	}
	doldB <- as.ddmatrix(x=oldB, bldim=bldim)
	if (comm.rank() == 0){
		newB <- matrix(c(0, 0))
	} else {
		newB <- NULL
	}
	dnewB <- as.ddmatrix(x=newB, bldim=bldim)
	nIter <- 0
	truemat <- ddmatrix(TRUE, bldim=bldim)
	comm.print(nIter)
	## Danger with predicates is that they too are dense distributed
	## matrices, and can't be &&'ed with logical vectors
	cont <- TRUE
	while (cont) {
		doldB <- dnewB
	## N-R as RWLS
		W <- weights(X, doldB, y)
		hessian <- - t(X) %*% W %*% X
		z <- X %*% doldB + solve(W) %*% (y - pr(X, doldB))
		dnewB <- solve(-hessian) %*% crossprod(X, W %*% z)
	##
		nIter <- nIter + 1
		comm.print(nIter)
		cont <- all.equal(colSums((dnewB - doldB)^2) > tolerance, truemat) &&
	       nIter < maxIter
	}
	dnewB
}

dHatB <- logReg(dX, dy, tolerance=1E-6, maxIter=100)
HatB <- as.matrix(dHatB, proc.dest=0)
comm.print(HatB, rank.print=0)

finalize()
