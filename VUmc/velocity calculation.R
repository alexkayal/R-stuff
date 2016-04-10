## a non-successful attempt at 
#  calculating velocity, assuming v0 = 0
##

velocity <- data.frame()
vx =0; vy=0;
for (i in 1:nrow(avgacc)){
  
  vx = vx+ avgacc$X[i]*averageMs/1000
  vy = vy+ avgacc$Y[i]*averageMs/1000
  v = sqrt(vx+vy)
  velocity <- rbind(velocity, c(vx,vy,v))
  
}

colnames(velocity) <- c("vx","vy","v")
plot(velocity$v*3.6, type="l")