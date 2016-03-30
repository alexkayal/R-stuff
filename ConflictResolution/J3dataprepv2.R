#J3 data preparation v2
##--- functions
reverseLastThird <- function(series, startAt){
  
  skipcounter <- 0;
  temp <-0;
  
  for (i in 1:nrow(series)){
    
    if (series$pp[i]>=startAt && 
        series$userPref01[i] == 1 
        && skipcounter%%2==0) {
      
      # reversal process
      temp <- series$total.S.A[i]
      series$total.S.A[i] <- series$total.S.B[i]
      series$total.S.B[i] <- temp
      
      if (series$norm.type[i]=="obl") 
        series$norm.type[i]<-"proh" 
      else
        series$norm.type[i]<-"obl" 
      
      series$userPref[i] <- series$userPref[i]*-1
      series$userPref01[i] <- abs(series$userPref01[i]-1)
      
      series$Frnd.before[i] <- series$Frnd.before[i]*-1
      series$Priv.before[i] <- series$Priv.before[i]*-1
      series$Saf.before[i] <- series$Saf.before[i]*-1
      series$Ind.before[i] <- series$Ind.before[i]*-1
      series$Res.before[i] <- series$Res.before[i]*-1
      
      series$Frnd.avg[i] <- series$Frnd.avg[i]*-1
      series$Priv.avg[i] <- series$Priv.avg[i]*-1
      series$Saf.avg[i] <- series$Saf.avg[i]*-1
      series$Ind.avg[i] <- series$Ind.avg[i]*-1
      series$Res.avg[i] <- series$Res.avg[i]*-1
      
    }
    
    skipcounter <- skipcounter + 1
    
  }
  
  return(series)
  
}

#---

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
colnames(stack1)[15] <- "gender"; colnames(stack1)[16] <- "hasKids";

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

# calculating averages of the two pies
total$Frnd.avg = (total$Frnd.before+total$Frnd.after)/2
total$Priv.avg = (total$Priv.before+total$Priv.after)/2
total$Saf.avg = (total$Frnd.before+total$Saf.after)/2
total$Ind.avg = (total$Priv.before+total$Ind.after)/2
total$Res.avg = (total$Priv.before+total$Res.after)/2
