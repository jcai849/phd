library(parallel)
library(uuid)

promise <- function(executor) {
    callCC(function(k) {
        resolve <- function(value) {
            print("resolving")
            k(value)
            print("I shouldn't be here")
            }
        reject <- function(value) {
            print("rejecting")
            k(value)
            print("I shouldn't be here")
            }
        executor(resolve, reject)
    })
    system(paste0("touch ", UUIDgenerate())) # test - should only run once!
}

promise(function(resolve, reject) {
    mcparallel({
        value <- 1+1
        resolve(value)
        print("I shouldn't be here")
    }, detached = TRUE)
})
