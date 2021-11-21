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
