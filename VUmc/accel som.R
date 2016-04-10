set.seed(7)

# calculating norm vector in case needed
nv <- 0
for(i in 1:nrow(avgacc))
  nv[i] <- norm(as.matrix(avgacc[i,]), "f")
nv <- data.frame(nv)

samplesize <- 170 #training sample size

# constructing samples for training and testing
training <- sample(nrow(avgacc), samplesize)

Xtraining <- scale(avgacc[training, ])
Xtest <- scale(avgacc[-training, ],
               center = attr(Xtraining, "scaled:center"),
               scale = attr(Xtraining, "scaled:scale"))

som.accel <- som(Xtraining, grid = somgrid(9, 9, "hexagonal"))

summary(som.accel)

# predictions
som.prediction <- predict(som.accel, newdata = Xtest,
                          trainX = Xtraining,
                          trainY = factor(accel.classes[training]))
table(accel.classes[-training], som.prediction$prediction)



