#J3 data preparation
#imports
libs <- c("xlsx", "geepack", "glmm", "lme4", "nlme")
lapply(libs, require, character.only = TRUE)

#Loading csv files
temp = list.files(pattern="pie*")
for (i in 1:length(temp)) assign(temp[i], read.csv(temp[i]))
J3 <- read.csv("confResLimited.csv", sep =";")

#specifying role: parent or child
role <- rep("child",nrow(J3))
for (i in 1:nrow(J3))
  if (J3$s1a[i]==1 || J3$s1a[i]==4 || J3$s1a[i]==5 || J3$s1a[i]==7 ||
      J3$s1a[i]==10 || J3$s1a[i]==12 || J3$s1a[i]==15 ||J3$s1a[i]==16) 
    role[i]="parent"

#Forming longitudunal data
stackAB <- data.frame(pie_A.csv,pie_B.csv)
colnames(stackAB)[1] <- "Frnd.before"; colnames(stackAB)[2] <- "Priv.before";
colnames(stackAB)[3] <- "Saf.before"; colnames(stackAB)[4] <- "Ind.before";
colnames(stackAB)[5] <- "Res.before"
colnames(stackAB)[6] <- "Frnd.after"; colnames(stackAB)[7] <- "Priv.after";
colnames(stackAB)[8] <- "Saf.after"; colnames(stackAB)[9] <- "Ind.after";
colnames(stackAB)[10] <- "Res.after"

stack1 <-  data.frame(J3$pp,J3$s1a,J3$s1b,J3$r1,pie_1.csv,pie_2.csv,J3$gender,J3$hasKids,role)
colnames(stack1)[1] <- "pp"; colnames(stack1)[2] <- "S.A";
colnames(stack1)[3] <- "S.B"; colnames(stack1)[4] <- "userPref";
colnames(stack1)[5] <- "Frnd.A"; colnames(stack1)[6] <- "Priv.A";
colnames(stack1)[7] <- "Saf.A"; colnames(stack1)[8] <- "Ind.A";
colnames(stack1)[9] <- "Res.A"
colnames(stack1)[10] <- "Frnd.B"; colnames(stack1)[11] <- "Priv.B";
colnames(stack1)[12] <- "Saf.B"; colnames(stack1)[13] <- "Ind.B";
colnames(stack1)[14] <- "Res.B"
colnames(stack1)[15] <- "gender"; colnames(stack1)[16] <- "hasKids"

stack2 <-  data.frame(J3$pp,J3$s2a,J3$s2b,J3$r2,pie_3.csv,pie_4.csv,J3$gender,J3$hasKids,role)
colnames(stack2)[1] <- "pp"; colnames(stack2)[2] <- "S.A";
colnames(stack2)[3] <- "S.B"; colnames(stack2)[4] <- "userPref";
colnames(stack2)[5] <- "Frnd.A"; colnames(stack2)[6] <- "Priv.A";
colnames(stack2)[7] <- "Saf.A"; colnames(stack2)[8] <- "Ind.A";
colnames(stack2)[9] <- "Res.A"
colnames(stack2)[10] <- "Frnd.B"; colnames(stack2)[11] <- "Priv.B";
colnames(stack2)[12] <- "Saf.B"; colnames(stack2)[13] <- "Ind.B";
colnames(stack2)[14] <- "Res.B"
colnames(stack2)[15] <- "gender"; colnames(stack2)[16] <- "hasKids"

stack3 <-  data.frame(J3$pp,J3$s3a,J3$s3b,J3$r3,pie_5.csv,pie_6.csv,J3$gender,J3$hasKids,role)
colnames(stack3)[1] <- "pp"; colnames(stack3)[2] <- "S.A";
colnames(stack3)[3] <- "S.B"; colnames(stack3)[4] <- "userPref";
colnames(stack3)[5] <- "Frnd.A"; colnames(stack3)[6] <- "Priv.A";
colnames(stack3)[7] <- "Saf.A"; colnames(stack3)[8] <- "Ind.A";
colnames(stack3)[9] <- "Res.A"
colnames(stack3)[10] <- "Frnd.B"; colnames(stack3)[11] <- "Priv.B";
colnames(stack3)[12] <- "Saf.B"; colnames(stack3)[13] <- "Ind.B";
colnames(stack3)[14] <- "Res.B"
colnames(stack3)[15] <- "gender"; colnames(stack3)[16] <- "hasKids"

stack4 <-  data.frame(J3$pp,J3$s4a,J3$s4b,J3$r4,pie_7.csv,pie_8.csv,J3$gender,J3$hasKids,role)
colnames(stack4)[1] <- "pp"; colnames(stack4)[2] <- "S.A";
colnames(stack4)[3] <- "S.B"; colnames(stack4)[4] <- "userPref";
colnames(stack4)[5] <- "Frnd.A"; colnames(stack4)[6] <- "Priv.A";
colnames(stack4)[7] <- "Saf.A"; colnames(stack4)[8] <- "Ind.A";
colnames(stack4)[9] <- "Res.A"
colnames(stack4)[10] <- "Frnd.B"; colnames(stack4)[11] <- "Priv.B";
colnames(stack4)[12] <- "Saf.B"; colnames(stack4)[13] <- "Ind.B";
colnames(stack4)[14] <- "Res.B"
colnames(stack4)[15] <- "gender"; colnames(stack4)[16] <- "hasKids"

total <-rbind(data.frame(stack1,stackAB),data.frame(stack2,stackAB),
              data.frame(stack3,stackAB),data.frame(stack4,stackAB))
total <- total[with(total, order(pp)), ]

# calculating prediction, using all values and the before pie
d1.before <- sqrt((total$Frnd.A-total$Frnd.before)^2 +
           (total$Priv.A-total$Priv.before)^2 +
           (total$Saf.A-total$Saf.before)^2 +
           (total$Ind.A-total$Ind.before)^2 +
           (total$Res.A-total$Res.before)^2)

d2.before <- sqrt((total$Frnd.B-total$Frnd.before)^2 +
             (total$Priv.B-total$Priv.before)^2 +
             (total$Saf.B-total$Saf.before)^2 +
             (total$Ind.B-total$Ind.before)^2 +
             (total$Res.B-total$Res.before)^2)

prediction.before <- d1.before - d2.before #positive means second agreement more favorable

# calculating prediction, using all values and the after pie
d1.after <- sqrt((total$Frnd.A-total$Frnd.after)^2 +
                    (total$Priv.A-total$Priv.after)^2 +
                    (total$Saf.A-total$Saf.after)^2 +
                    (total$Ind.A-total$Ind.after)^2 +
                    (total$Res.A-total$Res.after)^2)

d2.after <- sqrt((total$Frnd.B-total$Frnd.after)^2 +
                    (total$Priv.B-total$Priv.after)^2 +
                    (total$Saf.B-total$Saf.after)^2 +
                    (total$Ind.B-total$Ind.after)^2 +
                    (total$Res.B-total$Res.after)^2)

prediction.after <- d1.after - d2.after #positive means second agreement more favorable

# calculating averages of the two pies
total$Frnd.avg = (total$Frnd.before+total$Frnd.after)/2
total$Priv.avg = (total$Priv.before+total$Priv.after)/2
total$Saf.avg = (total$Frnd.before+total$Saf.after)/2
total$Ind.avg = (total$Priv.before+total$Ind.after)/2
total$Res.avg = (total$Priv.before+total$Res.after)/2

# calculating prediction, using all values and the average of the two pies
d1.avg <- sqrt((total$Frnd.A-total$Frnd.avg)^2 +
                   (total$Priv.A-total$Priv.avg)^2 +
                   (total$Saf.A-total$Saf.avg)^2 +
                   (total$Ind.A-total$Ind.avg)^2 +
                   (total$Res.A-total$Res.avg)^2)

d2.avg <- sqrt((total$Frnd.B-total$Frnd.avg)^2 +
                   (total$Priv.B-total$Priv.avg)^2 +
                   (total$Saf.B-total$Saf.avg)^2 +
                   (total$Ind.B-total$Ind.avg)^2 +
                   (total$Res.B-total$Res.avg)^2)

prediction.avg <- d1.avg - d2.avg #positive means second agreement more favorable

# prediction using individual values, and the before pie
Frnd.before = sqrt((total$Frnd.A-total$Frnd.before)^2) - sqrt((total$Frnd.B-total$Frnd.before)^2)
Priv.before = sqrt((total$Priv.A-total$Priv.before)^2) - sqrt((total$Priv.B-total$Priv.before)^2)
Saf.before = sqrt((total$Saf.A-total$Saf.before)^2) - sqrt((total$Saf.B-total$Saf.before)^2)
Ind.before = sqrt((total$Ind.A-total$Ind.before)^2) - sqrt((total$Ind.B-total$Ind.before)^2)
Res.before = sqrt((total$Res.A-total$Res.before)^2) - sqrt((total$Res.B-total$Res.before)^2)

# prediction using individual values, and the average pie
Frnd.avg = sqrt((total$Frnd.A-total$Frnd.avg)^2) - sqrt((total$Frnd.B-total$Frnd.avg)^2)
Priv.avg = sqrt((total$Priv.A-total$Priv.avg)^2) - sqrt((total$Priv.B-total$Priv.avg)^2)
Saf.avg = sqrt((total$Saf.A-total$Saf.avg)^2) - sqrt((total$Saf.B-total$Saf.avg)^2)
Ind.avg = sqrt((total$Ind.A-total$Ind.avg)^2) - sqrt((total$Ind.B-total$Ind.avg)^2)
Res.avg = sqrt((total$Res.A-total$Res.avg)^2) - sqrt((total$Res.B-total$Res.avg)^2)

# creating prediction data frame
prediction.data <- data.frame(total$pp,total$gender,total$hasKids,total$userPref,
                              total$S.A, total$S.B,
                              Frnd.before, Priv.before, Saf.before, Ind.before, Res.before,
                              Frnd.avg, Priv.avg, Saf.avg, Ind.avg, Res.avg,
                              prediction.before,prediction.after,prediction.avg, total$role)

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

#reversing to remove 2nd agreement bias; comment if you want to keep original values
# for (i in 1:nrow(prediction.data)){
#   if (prediction.data$total.S.A[i]>8){
#     prediction.data$userPref[i] <- prediction.data$userPref[i]*-1
#     prediction.data$prediction.before[i] <- prediction.data$prediction.before[i]*-1
#     prediction.data$prediction.after[i] <- prediction.data$prediction.after[i]*-1
#     prediction.data$prediction.avg[i] <- prediction.data$prediction.avg[i]*-1
#     
#     prediction.data$Frnd.before[i] <- prediction.data$Frnd.before[i]*-1
#     prediction.data$Priv.before[i] <- prediction.data$Priv.before[i]*-1
#     prediction.data$Saf.before[i] <- prediction.data$Saf.before[i]*-1
#     prediction.data$Ind.before[i] <- prediction.data$Ind.before[i]*-1
#     prediction.data$Res.before[i] <- prediction.data$Res.before[i]*-1
#   }
# }

# removing the obl/proh bias; comment if you want to keep original values
# for (i in 1:nrow(prediction.data)){
# 
#   # check the average, if still below zero and this row is below zero, reverse it
#   if (mean(prediction.data$userPref)<0 && prediction.data$userPref[i]<0) {
#   
#   prediction.data$userPref[i] <- prediction.data$userPref[i]*-1
#   prediction.data$prediction.before[i] <- prediction.data$prediction.before[i]*-1
#   prediction.data$prediction.after[i] <- prediction.data$prediction.after[i]*-1
#   prediction.data$prediction.avg[i] <- prediction.data$prediction.avg[i]*-1
#   
#   prediction.data$Frnd.before[i] <- prediction.data$Frnd.before[i]*-1
#   prediction.data$Priv.before[i] <- prediction.data$Priv.before[i]*-1
#   prediction.data$Saf.before[i] <- prediction.data$Saf.before[i]*-1
#   prediction.data$Ind.before[i] <- prediction.data$Ind.before[i]*-1
#   prediction.data$Res.before[i] <- prediction.data$Res.before[i]*-1
#   }
# }

# creating the 0-1 columns from silder/prediction data
for(i in 1:nrow(prediction.data)){
  if (prediction.data$userPref[i]>=0)
    prediction.data$userPref01[i] = 1
  else
    prediction.data$userPref01[i] = 0
  
  if (prediction.data$prediction.before[i]>=0)
    prediction.data$prediction.before01[i] = 1
  else
    prediction.data$prediction.before01[i] = 0
  
  if (prediction.data$prediction.after[i]>=0)
    prediction.data$prediction.after01[i] = 1
  else
    prediction.data$prediction.after01[i] = 0
  
  if (prediction.data$prediction.avg[i]>=0)
    prediction.data$prediction.avg1[i] = 1
  else
    prediction.data$prediction.avg1[i] = 0
  
}

# creating a limited table with 50/50
prediction.data.limited<- prediction.data[which(prediction.data$userPref<0 
                                                | (prediction.data$userPref>=0 & 
                                                     prediction.data$pp<224)), ]


#prediction.data$userPref1=as.factor(prediction.data$userPref1)
#prediction.data$prediction.before1=as.factor(prediction.data$prediction.before1)

prediction.data.hasKids <- prediction.data[prediction.data$hasKids=="yes",]
prediction.data.hasKids.parent <- prediction.data.hasKids[prediction.data.hasKids$role=="parent",]

# creating tables for individual conflicts, maybe i should add values from pies too
a110 <- prediction.data[prediction.data$pair=="1vs10",]
a110.limited<- a110[which(a110$userPref<0 | (a110$userPref>=0 & a110$pp<278)), ]

a29 <- prediction.data[prediction.data$pair=="2vs9",]
a29.limited<- a29[which(a29$userPref<0 | (a29$userPref>=0 & a29$pp<193)), ]

a311 <- prediction.data[prediction.data$pair=="3vs11",]
a311.limited<- a311[which(a311$userPref<0 | (a311$userPref>=0 & a311$pp<228)), ]

a412 <- prediction.data[prediction.data$pair=="4vs12",]
a412.limited<- a412[which(a412$userPref<0 | (a412$userPref>=0 & a412$pp<165)), ]

a516 <- prediction.data[prediction.data$pair=="5vs16",]
a516.limited<- a516[which(a516$userPref<0 | (a516$userPref>=0 & a516$pp<262)), ]

a614 <- prediction.data[prediction.data$pair=="6vs14",]
a614.limited<- a614[which(a614$userPref<0 | (a614$userPref>=0 & a614$pp<230)), ]

a715 <- prediction.data[prediction.data$pair=="7vs15",]
a715.limited<- a715[which(a715$userPref<0 | (a715$userPref>=0 & a715$pp<275)), ]

a813 <- prediction.data[prediction.data$pair=="8vs13",]
a813.limited<- a813[which(a813$userPref<0 | (a813$userPref>=0 & a813$pp<178)), ]
