# Load the kohonen package 
require(kohonen)

data(wines)
set.seed(7)
training <- sample(nrow(wines), 120)
Xtraining <- scale(wines[training, ])
Xtest <- scale(wines[-training, ],
               center = attr(Xtraining, "scaled:center"),
               scale = attr(Xtraining, "scaled:scale"))

som.wines <- som(Xtraining, grid = somgrid(5, 5, "hexagonal"))
som.prediction <- predict(som.wines, newdata = Xtest,
                            trainX = Xtraining,
                            trainY = factor(wine.classes[training]))
table(wine.classes[-training], som.prediction$prediction)

colors <- c("red", "green", "blue")
plot(som.wines, type = "mapping", col = colors[wine.classes],
     pch = wine.classes, main = "wines", keepMargins = TRUE)

####

data(yeast)
yeast.supersom <- supersom(yeast, somgrid(6, 6, "hexagonal"), whatmap = 3:6)
obj.classes <- as.integer(yeast$class)
colors <- c("yellow", "green", "blue", "red", "orange")
plot(yeast.supersom, type = "mapping", col = colors[obj.classes],
     pch = obj.classes, main = "yeast data", keepMargins = TRUE)
