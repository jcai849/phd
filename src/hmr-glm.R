if (!require(iotools)) install.packages('iotools',,'http://www.rforge.net/')
if (!require(hmr)) install.packages('hmr',,'http://www.rforge.net/')
if (!require(roctopus)) {
	system2("git", c("clone", "https://github.com/s-u/roctopus.git",
			 "~/roctopus"))
	install.packages("roctopus", repos=NULL)
}
## ------------------------------------------------------------------------
Sys.setenv(HADOOP_PREFIX="/usr/hdp/current/hadoop-client")

# was having intermittent permissions issues on hdp2 again,
# so made copies of these:
data_file <- "/user/taylor/AirlineDataSmall.csv"
data_file <- "/user/taylor/AirlineDataReallySmall.csv"
data_file <- "/user/kane/1987.csv"

header <- c('Year', 'Month', 'DayofMonth', 'DayOfWeek', 'DepTime',
            'CRSDepTime', 'ArrTime', 'CRSArrTime', 'UniqueCarrier',
            'FlightNum', 'TailNum', 'ActualElapsedTime', 'CRSElapsedTime',
            'AirTime', 'ArrDelay', 'DepDelay', 'Origin', 'Dest',
            'Distance', 'TaxiIn', 'TaxiOut', 'Cancelled',
            'CancellationCode', 'Diverted', "CarrierDelay",
            "WeatherDelay", "NASDelay", "SecurityDelay",
            "LateAircraftDelay")

clean_up_columns <- function(x) {
  x$Year <- factor(x$Year, levels=1987:2008)
  x$Month <- factor(x$Month, levels=1:12)
  x$DayOfWeek <- factor(x$DayOfWeek, 1:7)
  x$DepTime <- as.numeric(x$DepTime)
  x$DepTime <- fix_times(x$DepTime)
  x$DepDelay <- as.numeric(x$DepDelay)
  x$DepDelay[is.na(x$DepDelay)] <- 0
  x$DepDelay[x$DepDelay < 0] <- 0
  x$ArrDelay <- as.numeric(x$ArrDelay)
  x$Late <- as.numeric(x$ArrDelay > 15)
  x
}

fix_times <- function(t) {
  t <- as.character(t)
  l4 <- nchar(t) == 4
  l3 <- !l4
  ret <- rep(0, length(t))
  ret[l4] <- as.numeric(substr(t[l4], 1, 2)) * 60 +
    as.numeric(substr(t[l4], 3, 4))
  ret[l3] <- as.numeric(substr(t[l3], 1, 1)) * 60 +
    as.numeric(substr(t[l3], 2, 3))
  ret
}

a = hmr(hinput(data_file,
         function(x) mstrsplit(x, ",")),
  wait=TRUE, persistent=TRUE,
  aux = list(header=header, clean_up_columns=clean_up_columns, fix_times=fix_times),
  map = function(m) {

    update_vals <<- function(beta_hat) {
      beta <<- drop(mm %*% beta_hat) + OFFSET
      mu <- fam$linkinv(eta)
      mu.eta.val <- fam$mu.eta(eta)
      z <- (eta - OFFSET) + (y - mu) / mu.eta.val
      w <- sqrt((WEIGHTS * mu.eta.val^2)/fam$variance(mu))
      return(0L)
    }

    res <- try({
      form <- Late ~ DepDelay + DepTime + DayOfWeek

      df <- as.data.frame(m)
      names(df) <- header
      df <- clean_up_columns(df)

      fam <<- binomial()
      mm <<- model.matrix(form, df)
      y <<- matrix(df[rownames(mm), all.vars(form)[1]], ncol=1)
      .GlobalEnv$beta <- NULL
      NOBS <<- NROW(y)
      WEIGHTS <<- rep.int(1, NROW(y))
      OFFSET <<- rep.int(0, NROW(y))
      #eval(fam$initialize)
      mustart <<- (WEIGHTS * y + 0.5)/(WEIGHTS + 1)
      eta <<- fam$linkfun(mustart)
      mu <<- fam$linkinv(eta)
      mu.eta.val <<- fam$mu.eta(eta)
      z <<- (eta - OFFSET) + (y - mu) / mu.eta.val
      w <<- sqrt((WEIGHTS * mu.eta.val^2)/fam$variance(mu))

    }, silent=TRUE)

    error <<- "none"
    if (inherits(res, "try-error"))
      error <<- as.character(res)
  }
)

x = readLines(open(a))
w = lapply(x, worker)

epsilon = 1e-08
maxit = 8

## ------------------------------------------------------------------------
# Cluster implementation:
dev = wqapply(w, sum(fam$dev.resids(y, mu, WEIGHTS)), fold=`+`)

for (iter in 1L:maxit) {
  XtX = wqapply(w, crossprod( mm[, , drop = FALSE] * as.numeric(w) ), fold=`+`)

  Xty = wqapply(w, t(mm[, , drop = FALSE] * as.numeric(w)) %*% (z * as.numeric(w)), fold=`+`)

  beta = solve(XtX, Xty)

  wrun(w, bquote(update_vals(.(beta))))
  
  devold = dev
  dev = wqapply(w, sum(fam$dev.resids(y, mu, WEIGHTS)), fold=`+`)

  cat("Deviance = ", dev, " Iterations - ", iter, "\n", sep = "")
  if (abs(dev - devold)/(0.1 + abs(dev)) < epsilon) break;
}


## ------------------------------------------------------------------------
# Local version
weval(w[[2]], identity(fam), wait=FALSE)
l_fam = wresult(w[[2]])

# Get these from the workers
for(i in w) weval(i, identity(mm), wait=FALSE)
l_mm = do.call(rbind, lapply(w, wresult))
for(i in w) weval(i, identity(y), wait=FALSE)
l_y = do.call(c, lapply(w, wresult))

# Close the connections now
lapply(w, wclose)

# Local result
gout = glm.fit(l_mm, l_y, family=l_fam, control=glm.control(trace=TRUE))

# Predicted coefficients
gout$coefficients

# Check against distributed version
max(abs(gout$coefficients - drop(beta)))
gout$deviance - dev


