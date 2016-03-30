#correlations

fourth_indexes <-seq(1,1584,4)

Frnd.cor <- data.frame(total$Frnd.before, total$Frnd.after)/72
Priv.cor <- data.frame(total$Priv.before, total$Priv.after)/72
Saf.cor <- data.frame(total$Saf.before, total$Saf.after)/72
Ind.cor <- data.frame(total$Ind.before, total$Ind.after)/72
Res.cor <- data.frame(total$Res.before, total$Res.after)/72

Frnd.cor <- Frnd.cor[fourth_indexes,]
Priv.cor <- Priv.cor[fourth_indexes,]

cor(Frnd.cor)
cor(Priv.cor)
cor(Saf.cor)
cor(Ind.cor)
cor(Res.cor)
