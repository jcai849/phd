options(citation.bibtex.max=999)

noncran <- c("distributedR", "RHadoop", "RHIPE", "hmr", "bigalgebra",
             "ffbase2")
cran <- c("base", "foreach", "doParallel", "doSNOW", "doMPI",
          "future", "furrr", "partools", "pbdBASE", "Rmpi",
          "sparklyr", "SparkR", "doFuture", "future.batchtools",
          "future.apply", "future.callr", "batchtools", "callr",
          "snow", "bigmemory", "biganalytics", "bigtabulate",
          "biglasso", "bigstatsr", "disk.frame", "data.table", "fst",
          "iotools", "ff", "biglm", "ffbase")

install.packages(x)
## install.packages(cran)
## install.packages("hmr",,"http://rforge.net")
## install.packages(c("RSclient","Rserve"), repos = "https://rforge.net")
## install.packages("big.data.table", repos = "http://jangorecki.gitlab.io/big.data.table")
## devtools::install_github("edwindj/ffbase2")

sink("r-packages.txt")
sapply(c(cran, noncran),
       function(x)toBibtex(citation(x)))
sink()

## make vectorised version of citation outputting only bibtex
