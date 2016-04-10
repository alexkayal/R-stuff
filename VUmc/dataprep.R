## reading accelerometer data
acc1 <- read.csv("acc1.csv")
acc2 <- read.csv("acc2.csv")
acc3 <- read.csv("acc3.csv")
acc <- rbind (acc1, acc2, acc3)
 
# plotting
plot (acc$X, type ="l")
lines (acc$Y, type ="l", col = "red")
lines (acc$Z, type ="l", col = "green")

## averaging
avgacc <- data.frame()
ms = 0; index = 1; averageMs <- 15000;

for (i in 1:nrow(acc)) {
  
  ms = ms+acc$t[i]
  if (ms>averageMs){
    
    X <- mean(acc$X[index:i])
    Y <- mean(acc$Y[index:i])
    Z <- mean(acc$Z[index:i])
    avgacc <- rbind(avgacc,c(X,Y,Z))
    
    ms <-0
    index <- i
  }
}

colnames(avgacc) <- c("X","Y","Z")

plot(avgacc$X, type ="l")
lines(avgacc$Y, type ="l", col ="red")

## annotations
# class 0 = sedentary
# class 1 = very light activity
# class 2 = moderate activity

accel.classes <- 0
accel.classes [1:39] <- 2
accel.classes [40:50] <- 0
accel.classes [51:52] <- 2
accel.classes [53:59] <- 0
accel.classes [60:69] <- 2
accel.classes [70:107] <- 0
accel.classes [108:209] <- 1
accel.classes [210:253] <- 2