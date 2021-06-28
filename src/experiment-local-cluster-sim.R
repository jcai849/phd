## Much of the difficulty of this is two-part:

## 1. using the incorrect OOP style; I have been trying to force S4
## into more of a class-based object system like python. If I want to
## continue with that style, I should be using reference classes. If I
## want to use S4 as intended, I should be defining replacer functions
## instead of direct state-changing functions by call.

## 2. Lack of familiarity with quotation. I should explicitly state
## what I want to be done, such as, "evaluate an expression given as
## an argument to an outer function, by an environment referenced in
## an inner function using eval". Possibly draw it. Learn more on
## expressions and calls through, e.g., Advanced R, while
## experimenting and recording experimentation of the main quoting
## functions (documented in LaTeX, not R comments. This should be done
## with a firmer knowledge of debugging in R; More information should
## be available for me specifically in the ESS manual, alongside
## knowledge of the debug family of functions. Failing all of that,
## the R mailing list should be consulted (or Stack Overflow??).

node <- setClass("node",
                 slots = c(name = "character",
                           env = "environment"))

## method.skeleton("initialize", "node")

## Constructor
setMethod("initialize",
          signature(.Object = "node"),
          function (.Object, name,...) {
              .Object@name <- name
              .Object@env <- new.env(parent = baseenv())
              .Object})

## setters

setGeneric("push",
           function(.Object, ...){
               standardGeneric("push")})

setMethod("push",
          signature(.Object = "node"),
          function(.Object, ...){
              arguments <- as.list(substitute(list(...)))
              defs <- names(arguments)[-1L]
              for (def in defs) {
                  assign(def, eval(arguments[[def]]), envir = .Object@env)}
          })


## getters
setMethod("names",
    signature(x = "node"),
    function (x) {
        x@name})

setGeneric("pull",
           function(.Object, ...){
               standardGeneric("pull")})

setMethod("pull",
          signature(.Object = "node"),
          function(.Object, ...){
              arguments <- as.list(substitute(list(...)))
              defs <- sapply(arguments, deparse)[-1L]
              obs <- lapply(defs, get, envir = .Object@env)
              names(obs) <- defs
              obs})

## mutaters
setGeneric("process",
           function(.Object, expr, ...){
               standardGeneric("process")})

setMethod("process",
          signature(.Object = "node"),
          function(.Object, expr, ...) {
              eval(substitute(expr),
                   .Object@env)
          })

## Process was intended to modify the environment through some
## arbitrary expression, being evaluated by the object environment,
## potentially creating bindings for it along the way. This has proved
## to be much harder than originally thought; environments seem very
## difficult to populate through evaluation. `sys.source` doesn't
## work, as it takes an environment as an argument, modifying a copy
## of the environment if it is in a function. If it is at the top
## level, it has the side-effect of modification. `with` returns the
## evaluation, the modification is local if inside a function.
## `assign` manages modification well in or out of a function, but
## requires named variables as arguments, rather than just arbitrary
## expressions.

## expr <- substitute({y <- 1})
## x <- list()
 ## x$env <- new.env(parent = baseenv())
## tmp <- tempfile()
## writeLines(deparse(expr), tmp)
## sys.source(tmp, x$env)
## unlink(tmp)
## ls(x$env)

## Running

x <- node("1")
push(x, k = 3, y = 2+2)
ls(x@env)
pull(x, y)
names(x)
process(x, {k + y})



distribute <- function(nodes, data){
    n.nodes <- length(nodes)
    data.split <- split(data, 1:length(data) %% n.nodes)
    for(i in 1:n.nodes) {
        push(nodes[[i]], dist = data.split[[i]])
    }
}

nodes.process <- function(nodes, expr){
    n.nodes <- length(nodes)
    ## processed <- rep(NA, n.nodes)
    ## for (i in 1:n.nodes) {
    ##     processed[i] <- process(nodes[[i]], substitute(expr))
    ## }
    expr <- deparse(substitute(expr))
    processed <- process(nodes[[1]], parse(expr))
    processed
}

nodes <- lapply(as.character(1:10), node)
data = 1:60
distribute(nodes, data)
ls({nodes[[1]]}@env)
(unlist(pull(nodes[[1]], dist)))

n.nodes  <- length(nodes)
processed <- rep(NA, n.nodes)
for (i in 1:n.nodes) {
    processed[i] <- process(nodes[[i]], {sum(dist)})
}
processed

test <- function(x){
    mean(x)
}

test <- function(x){    
    x <- x + 1
    y <- mean(x)
    x <- ifelse(x > 5, x, x - 100)
  list(x, y)
}

test <- 4
