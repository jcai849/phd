library(promises)
p1 <- promise_resolve(TRUE)
p2 <- promise(~later::later(~resolve(TRUE), delay = 2))
promise_all(p1, p2) %...>% str
cat("Print above if non-concurrent with main process, below if concurrent\n")

const p1 = Promise.resolve(true);
const p2 = new Promise((resolve, reject) => {
  setTimeout(resolve, 2000, true);
});
Promise.all([p1, p2]).then((values) => {
    console.log(values);
});
console.log("Print above if non-concurrent with main process, below if concurrent")
