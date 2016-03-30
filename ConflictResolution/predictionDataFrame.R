# prediction using individual values, and the before pie
Frnd.before = sqrt((total$Frnd.A-total$Frnd.before)^2) - sqrt((total$Frnd.B-total$Frnd.before)^2)
Priv.before = sqrt((total$Priv.A-total$Priv.before)^2) - sqrt((total$Priv.B-total$Priv.before)^2)
Saf.before = sqrt((total$Saf.A-total$Saf.before)^2) - sqrt((total$Saf.B-total$Saf.before)^2)
Ind.before = sqrt((total$Ind.A-total$Ind.before)^2) - sqrt((total$Ind.B-total$Ind.before)^2)
Res.before = sqrt((total$Res.A-total$Res.before)^2) - sqrt((total$Res.B-total$Res.before)^2)

# prediction using individual values, and the after pie
Frnd.after = sqrt((total$Frnd.A-total$Frnd.after)^2) - sqrt((total$Frnd.B-total$Frnd.after)^2)
Priv.after = sqrt((total$Priv.A-total$Priv.after)^2) - sqrt((total$Priv.B-total$Priv.after)^2)
Saf.after = sqrt((total$Saf.A-total$Saf.after)^2) - sqrt((total$Saf.B-total$Saf.after)^2)
Ind.after = sqrt((total$Ind.A-total$Ind.after)^2) - sqrt((total$Ind.B-total$Ind.after)^2)
Res.after = sqrt((total$Res.A-total$Res.after)^2) - sqrt((total$Res.B-total$Res.after)^2)

# prediction using individual values, and the average pie
Frnd.avg = sqrt((total$Frnd.A-total$Frnd.avg)^2) - sqrt((total$Frnd.B-total$Frnd.avg)^2)
Priv.avg = sqrt((total$Priv.A-total$Priv.avg)^2) - sqrt((total$Priv.B-total$Priv.avg)^2)
Saf.avg = sqrt((total$Saf.A-total$Saf.avg)^2) - sqrt((total$Saf.B-total$Saf.avg)^2)
Ind.avg = sqrt((total$Ind.A-total$Ind.avg)^2) - sqrt((total$Ind.B-total$Ind.avg)^2)
Res.avg = sqrt((total$Res.A-total$Res.avg)^2) - sqrt((total$Res.B-total$Res.avg)^2)

# prediction using concensus, and the before pie
Frnd.cons.before = sqrt((total$cons.Frnd.A-total$Frnd.before)^2) - sqrt((total$cons.Frnd.B-total$Frnd.before)^2)
Priv.cons.before = sqrt((total$cons.Priv.A-total$Priv.before)^2) - sqrt((total$cons.Priv.B-total$Priv.before)^2)
Saf.cons.before = sqrt((total$cons.Saf.A-total$Saf.before)^2) - sqrt((total$cons.Saf.B-total$Saf.before)^2)
Ind.cons.before = sqrt((total$cons.Ind.A-total$Ind.before)^2) - sqrt((total$cons.Ind.B-total$Ind.before)^2)
Res.cons.before = sqrt((total$cons.Res.A-total$Res.before)^2) - sqrt((total$cons.Res.B-total$Res.before)^2)

# creating prediction data frame
prediction.data <- data.frame(total$pp,total$gender,total$hasKids,total$userPref,
                              total$S.A, total$S.B, total$role,
                              Frnd.before, Priv.before, Saf.before, Ind.before, Res.before,
                              Frnd.after, Priv.after, Saf.after, Ind.after, Res.after,
                              Frnd.avg, Priv.avg, Saf.avg, Ind.avg, Res.avg,
                              Frnd.cons.before, Priv.cons.before, Saf.cons.before, Ind.cons.before, Res.cons.before)

colnames(prediction.data)[1] <- "pp"; 
colnames(prediction.data)[2] <- "gender"; 
colnames(prediction.data)[3] <- "hasKids"; 
colnames(prediction.data)[4] <- "userPref";
prediction.data$userPref <- prediction.data$userPref-5
prediction.data <- na.omit(prediction.data)

#creating scenario pairs column
for (i in 1:nrow(prediction.data)){
  if (prediction.data$total.S.A[i]==1 || prediction.data$total.S.A[i]==10)
    prediction.data$pair[i] <- "1vs10"
  if (prediction.data$total.S.A[i]==2 || prediction.data$total.S.A[i]==9)
    prediction.data$pair[i] <- "2vs9"
  if (prediction.data$total.S.A[i]==3 || prediction.data$total.S.A[i]==11)
    prediction.data$pair[i] <- "3vs11"
  if (prediction.data$total.S.A[i]==4 || prediction.data$total.S.A[i]==12)
    prediction.data$pair[i] <- "4vs12"
  if (prediction.data$total.S.A[i]==5 || prediction.data$total.S.A[i]==16)
    prediction.data$pair[i] <- "5vs16"
  if (prediction.data$total.S.A[i]==6 || prediction.data$total.S.A[i]==14)
    prediction.data$pair[i] <- "6vs14"
  if (prediction.data$total.S.A[i]==7 || prediction.data$total.S.A[i]==15)
    prediction.data$pair[i] <- "7vs15"
  if (prediction.data$total.S.A[i]==8 || prediction.data$total.S.A[i]==13)
    prediction.data$pair[i] <- "8vs13"
}

#norm type
for (i in 1:nrow(prediction.data)){
  if(prediction.data$total.S.A[i]<=8)
    prediction.data$norm.type[i] = "obl"
  else
    prediction.data$norm.type[i] = "proh"
}

# creating the 0-1 columns from silder
for(i in 1:nrow(prediction.data)){
  if (prediction.data$userPref[i]>=0)
    prediction.data$userPref01[i] = 1
  else
    prediction.data$userPref01[i] = 0
}

# creating tables for individual conflicts, maybe i should add values from pies too
a110 <- prediction.data[prediction.data$pair=="1vs10",]
a29 <- prediction.data[prediction.data$pair=="2vs9",]
a311 <- prediction.data[prediction.data$pair=="3vs11",]
a412 <- prediction.data[prediction.data$pair=="4vs12",]
a516 <- prediction.data[prediction.data$pair=="5vs16",]
a614 <- prediction.data[prediction.data$pair=="6vs14",]
a715 <- prediction.data[prediction.data$pair=="7vs15",]
a813 <- prediction.data[prediction.data$pair=="8vs13",]

# creating reversed last thirds
prediction.data.adj <- reverseLastThird(prediction.data, 227)
a110.adj <- reverseLastThird(a110, 281)
a29.adj <- reverseLastThird(a29, 206)
a311.adj <- reverseLastThird(a311, 234)
a412.adj <- reverseLastThird(a412, 145)
a516.adj <- reverseLastThird(a516, 262)
a614.adj <- reverseLastThird(a614, 232)
a715.adj <- reverseLastThird(a715, 277)
a813.adj <- reverseLastThird(a813, 178)

# creating truncated tables
a110.limited<- a110[which(a110$userPref<0 | (a110$userPref>=0 & a110$pp<278)), ]
a29.limited<- a29[which(a29$userPref<0 | (a29$userPref>=0 & a29$pp<193)), ]
a311.limited<- a311[which(a311$userPref<0 | (a311$userPref>=0 & a311$pp<228)), ]
a412.limited<- a412[which(a412$userPref<0 | (a412$userPref>=0 & a412$pp<165)), ]
a516.limited<- a516[which(a516$userPref<0 | (a516$userPref>=0 & a516$pp<262)), ]
a614.limited<- a614[which(a614$userPref<0 | (a614$userPref>=0 & a614$pp<230)), ]
a715.limited<- a715[which(a715$userPref<0 | (a715$userPref>=0 & a715$pp<275)), ]
a813.limited<- a813[which(a813$userPref<0 | (a813$userPref>=0 & a813$pp<178)), ]
