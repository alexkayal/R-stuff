library(nnet)

samp1 <- c(sample(1:nrow(avgacc),170)) # contiguous data points for training
samp2 <- (1:170) # random data points for training
samp <- samp2

# training
net <- nnet(avgacc[samp,], accel.classes[samp], size = 2, rang = 0.1,
            decay = 5e-4, maxit = 200)
# prediction
table(accel.classes[-samp], round(predict(net, avgacc[-samp,]),0))