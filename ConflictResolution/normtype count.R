count =0
for (i in 1:nrow(prediction.data)){ 
  
  if (prediction.data$norm.type[i]=="obl" && prediction.data$userPref01[i]==0) {
    prediction.data$norm_type[i] <- "obl"
  }
  
  else if (prediction.data$norm.type[i]=="proh" && prediction.data$userPref01[i]==1){
    prediction.data$norm_type[i] <- "obl"
  }
  else{
    prediction.data$norm_type[i] <- "proh"
    count = count +1
  }
  }