# Local Decision Tree

X <- mtcars[,c("vs", "am", "gear", "carb")]
y <- mtcars$cyl
tree <- dist_decision_tree(X, y)
predict(tree, X)
