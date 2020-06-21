# Local Decision Tree

X <- mtcars[,c("vs", "am", "gear", "carb")]
y <- mtcars$cyl
tree <- dist_decision_tree(X, y)
predict(tree, X)

# Distributed Decision Tree

hosts <- paste0("hadoop", 1:8)
rsc <- make_cluster(hosts)

dX <- as.distributed(mtcars[,c("vs", "am", "gear", "carb")], rsc)
dy <- as.distributed(mtcars$cyl, rsc)
dtree <- dist_decision_tree(dX, dy)
predict(dtree, X)

kill_servers(hosts)
