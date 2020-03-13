options(citation.bibtex.max=999)

noncran <- c("distributedR", "RHadoop", "RHIPE")
cran <- c("base", "foreach", "doParallel", "doSNOW", "doMPI",
          "future", "furrr", "partools", "pbdBASE", "Rmpi",
          "sparklyr", "SparkR")

## install.packages(cran)

sink("r-packages.txt")
sapply(cran, citation)
sink()

## make vectorised version of citation outputting only bibtex
