options(citation.bibtex.max=999)

noncran <- c("distributedR", "RHadoop", "RHIPE", "hmr", "bigalgebra",
             "ffbase2", "pbdDMAT", "pbdML", "hpcvis", "pbdCS",
             "doAzureParallel", "pbdSLAP", "RBigData/pmclust")
cran <- c("base", "foreach", "doParallel", "doSNOW", "doMPI",
          "future", "furrr", "partools", "pbdBASE", "Rmpi",
          "sparklyr", "SparkR", "doFuture", "future.batchtools",
          "future.apply", "future.callr", "batchtools", "callr",
          "snow", "bigmemory", "biganalytics", "bigtabulate",
          "biglasso", "bigstatsr", "disk.frame", "data.table", "fst",
          "iotools", "ff", "biglm", "ffbase", "pbdMPI", "doRNG",
          "doRedis", "promises", "graphframes", "rbenchmark", "microbenchmark",
	  "kazaam")

## install.packages(cran)
## remotes::install_github(c("wrathematics/getip", "RBigData/hpcvis", "RBigData/pbdCS", "RBigData/pbdDMAT", "RBigData/pbdML", "edwindj/ffbase2", "Azure/rAzureBatch", "Azure/doAzureParallel", "RBigData/pmclust", "RBigData/pbdSLAP"))
## install.packages(c("RSclient","Rserve", "hmr"), repos = "https://rforge.net")
## install.packages("big.data.table", repos = "http://jangorecki.gitlab.io/big.data.table")

sink("r-packages.txt")
sapply(x,
       function(x)toBibtex(citation(x)))
sink()

## make vectorised version of citation outputting only bibtex
