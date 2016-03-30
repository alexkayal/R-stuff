Frnd <-0; Priv <-0; Saf <-0; Ind <-0; Res <-0;
for (i in 1:16) {
  xA <- data.frame(total[which(total$S.A==i),])
  xA <- data.frame(xA$Frnd.A, xA$Priv.A, xA$Saf.A, xA$Ind.A, xA$Res.A)
  colnames(xA)[1] <- "Frnd"; colnames(xA)[2] <- "Priv";
  colnames(xA)[3] <- "Saf"; colnames(xA)[4] <- "Ind";
  colnames(xA)[5] <- "Res";
  xB <- data.frame(total[which(total$S.B==i),])
  xB <- data.frame(xB$Frnd.B, xB$Priv.B, xB$Saf.B, xB$Ind.B, xB$Res.B)
  colnames(xB)[1] <- "Frnd"; colnames(xB)[2] <- "Priv";
  colnames(xB)[3] <- "Saf"; colnames(xB)[4] <- "Ind";
  colnames(xB)[5] <- "Res";
  x <- rbind(xA,xB)
  Frnd[i] = mean(x$Frnd);
  Priv[i] = mean(x$Priv);
  Saf[i] = mean(x$Saf);
  Ind[i] = mean(x$Ind);
  Res[i] = mean(x$Res);
  Cons <- data.frame(Frnd,Priv,Saf,Ind,Res)
}
remove(Frnd,Priv,Saf,Ind,Res)

for (i in 1:nrow(total)){
  total$cons.Frnd.A[i] <- Cons$Frnd[total$S.A[i]]
  total$cons.Frnd.B[i] <- Cons$Frnd[total$S.B[i]]
  
  total$cons.Priv.A[i] <- Cons$Priv[total$S.A[i]]
  total$cons.Priv.B[i] <- Cons$Priv[total$S.B[i]]
  
  total$cons.Saf.A[i] <- Cons$Saf[total$S.A[i]]
  total$cons.Saf.B[i] <- Cons$Saf[total$S.B[i]]
  
  total$cons.Ind.A[i] <- Cons$Ind[total$S.A[i]]
  total$cons.Ind.B[i] <- Cons$Ind[total$S.B[i]]
  
  total$cons.Res.A[i] <- Cons$Res[total$S.A[i]]
  total$cons.Res.B[i] <- Cons$Res[total$S.B[i]]
  
}

# pie chart
j <- 16
slices <- c(Cons$Frnd[j], Cons$Priv[j], Cons$Saf[j], Cons$Ind[j], Cons$Res[j])
lbls <- c("Friendship", "Privacy", "Safety", "Independence", "Responsibility")
pie(slices, labels = lbls, main="Values")
