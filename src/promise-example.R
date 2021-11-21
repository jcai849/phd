#Implicit Asynchrony

plist <- lapply(prerequisites(request),
       function(chunk)
           promise(function(resolve, reject)
                       resolve(emerge(chunk))))
pall <- promise_all(p)
worked <- then(pall,
               onFulfilled=function(value)
                   promise(function(resolve, reject)
                               tryCatch(resolve(do.call(computation(request), value)),
                                        error=reject)))

# Explicit Asynchrony
plist <- lapply(prerequisites(request),
       function(chunk)
           promise(function(resolve, reject) mcparallel(
                           resolve(emerge(chunk)),
                       detached=TRUE)))
pall <- promise_all(p)
worked <- then(pall,
               onFulfilled=function(value)
                   promise(function(resolve, reject) mcparallel(
                                   tryCatch(resolve(do.call(computation(request),value)),
                                            error=reject)),
                               detached=TRUE))
