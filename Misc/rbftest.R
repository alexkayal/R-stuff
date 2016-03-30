#series <- periodReturn(IBM, period="weekly")
inputsize <-10
trainingsize <-500
testingsize <-20
hiddenlayers <-5

series <- EURUSD[200:1000,]
#series <- EURUSD$EURUSD.Close[1:1300,]
#series <- to.weekly(series)$series.Close

inputs <- as.matrix(coredata(Lag(series,k=1:inputsize))[(inputsize+1):trainingsize,])
outputs <- as.matrix(coredata(series)[(inputsize+1):trainingsize,])

test <- as.matrix(coredata(Lag(series,k=1:inputsize))[(trainingsize+1):(trainingsize+testingsize),])
target <- as.matrix(coredata(series)[(trainingsize+1):(trainingsize+testingsize)])

model <- rbf(inputs, outputs, size=hiddenlayers, maxit=100000
             , 
             initFuncParams=c(0, 1, 0, 0.01, 0.01),
             learnFuncParams=c(1e-8, 0, 1e-8, 0.1, 0.8), 
             linOut=TRUE)

prediction <- predict(model, test)

plot(target, type="l")
print (error <- abs(prediction-target))

