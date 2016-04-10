## glm data frame
glmdata <- data.frame(avgacc, accel.classes)

## glm model and summary
model.glm <- glm(accel.classes ~ X*Y, data = glmdata)
summary(model.glm)

## fitting and predictions
class <- data.frame(predicted = glmdata$accel.classes, response = round(fitted(model.glm),0))
xtabs(~ predicted + response, data = class)