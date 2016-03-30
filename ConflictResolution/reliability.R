require(psych)
require(MASS)
require(lte)
require(psy)

ppnum <- nrow(J3)
pieData <- rbind(pie_1.csv,pie_2.csv, 
                 pie_3.csv,pie_4.csv,
                 pie_5.csv,pie_6.csv,
                 pie_7.csv,pie_8.csv)

Frnd_rel <- NA
Priv_rel <- NA
Saf_rel <- NA
Ind_rel <- NA
Res_rel <- NA

for (i in 1:ppnum) {
  temp <-rep(NA, 16)
  
  temp[J3$s1a[i]] <- pieData$Frnd[i]
  temp[J3$s1b[i]] <- pieData$Frnd[i+ppnum]
  temp[J3$s2a[i]] <- pieData$Frnd[i+ppnum*2]
  temp[J3$s2b[i]] <- pieData$Frnd[i+ppnum*3]
  temp[J3$s3a[i]] <- pieData$Frnd[i+ppnum*4]
  temp[J3$s3b[i]] <- pieData$Frnd[i+ppnum*5]
  temp[J3$s4a[i]] <- pieData$Frnd[i+ppnum*6]
  temp[J3$s4b[i]] <- pieData$Frnd[i+ppnum*7]
  
  if (is.na(Frnd_rel))
    Frnd_rel <- temp
  else
    Frnd_rel <- data.frame(Frnd_rel, temp)
  ##--  
  temp <-rep(NA, 16)
  
  temp[J3$s1a[i]] <- pieData$Priv[i]
  temp[J3$s1b[i]] <- pieData$Priv[i+ppnum]
  temp[J3$s2a[i]] <- pieData$Priv[i+ppnum*2]
  temp[J3$s2b[i]] <- pieData$Priv[i+ppnum*3]
  temp[J3$s3a[i]] <- pieData$Priv[i+ppnum*4]
  temp[J3$s3b[i]] <- pieData$Priv[i+ppnum*5]
  temp[J3$s4a[i]] <- pieData$Priv[i+ppnum*6]
  temp[J3$s4b[i]] <- pieData$Priv[i+ppnum*7]
  
  if (is.na(Priv_rel))
    Priv_rel <- temp
  else
    Priv_rel <- data.frame(Priv_rel, temp)
  
  ##--  
  temp <-rep(NA, 16)
  
  temp[J3$s1a[i]] <- pieData$Saf[i]
  temp[J3$s1b[i]] <- pieData$Saf[i+ppnum]
  temp[J3$s2a[i]] <- pieData$Saf[i+ppnum*2]
  temp[J3$s2b[i]] <- pieData$Saf[i+ppnum*3]
  temp[J3$s3a[i]] <- pieData$Saf[i+ppnum*4]
  temp[J3$s3b[i]] <- pieData$Saf[i+ppnum*5]
  temp[J3$s4a[i]] <- pieData$Saf[i+ppnum*6]
  temp[J3$s4b[i]] <- pieData$Saf[i+ppnum*7]
  
  if (is.na(Saf_rel))
    Saf_rel <- temp
  else
    Saf_rel <- data.frame(Saf_rel, temp)
  
  ##--  
  temp <-rep(NA, 16)
  
  temp[J3$s1a[i]] <- pieData$Ind[i]
  temp[J3$s1b[i]] <- pieData$Ind[i+ppnum]
  temp[J3$s2a[i]] <- pieData$Ind[i+ppnum*2]
  temp[J3$s2b[i]] <- pieData$Ind[i+ppnum*3]
  temp[J3$s3a[i]] <- pieData$Ind[i+ppnum*4]
  temp[J3$s3b[i]] <- pieData$Ind[i+ppnum*5]
  temp[J3$s4a[i]] <- pieData$Ind[i+ppnum*6]
  temp[J3$s4b[i]] <- pieData$Ind[i+ppnum*7]
  
  if (is.na(Ind_rel))
    Ind_rel <- temp
  else
    Ind_rel <- data.frame(Ind_rel, temp)
  
  ##--  
  temp <-rep(NA, 16)

  temp[J3$s1a[i]] <- pieData$Res[i]
  temp[J3$s1b[i]] <- pieData$Res[i+ppnum]
  temp[J3$s2a[i]] <- pieData$Res[i+ppnum*2]
  temp[J3$s2b[i]] <- pieData$Res[i+ppnum*3]
  temp[J3$s3a[i]] <- pieData$Res[i+ppnum*4]
  temp[J3$s3b[i]] <- pieData$Res[i+ppnum*5]
  temp[J3$s4a[i]] <- pieData$Res[i+ppnum*6]
  temp[J3$s4b[i]] <- pieData$Res[i+ppnum*7]

  if (is.na(Res_rel))
    Res_rel <- temp
  else
    Res_rel <- data.frame(Res_rel, temp)

}


Frnd_rel.parent <- Frnd_rel[which(is.na(Frnd_rel$temp.3)==FALSE),]
Frnd_rel.parent <- Frnd_rel.parent[,colSums(is.na(Frnd_rel.parent))<nrow(Frnd_rel.parent)]
Frnd_rel.child <- Frnd_rel[which(is.na(Frnd_rel$temp)==FALSE),]
Frnd_rel.child <- Frnd_rel.child[,colSums(is.na(Frnd_rel.child))<nrow(Frnd_rel.child)]

Priv_rel.parent <- Priv_rel[which(is.na(Priv_rel$temp.3)==FALSE),]
Priv_rel.parent <- Priv_rel.parent[,colSums(is.na(Priv_rel.parent))<nrow(Priv_rel.parent)]
Priv_rel.child <- Priv_rel[which(is.na(Priv_rel$temp)==FALSE),]
Priv_rel.child <- Priv_rel.child[,colSums(is.na(Priv_rel.child))<nrow(Priv_rel.child)]

Saf_rel.parent <- Saf_rel[which(is.na(Saf_rel$temp.3)==FALSE),]
Saf_rel.parent <- Saf_rel.parent[,colSums(is.na(Saf_rel.parent))<nrow(Saf_rel.parent)]
Saf_rel.child <- Saf_rel[which(is.na(Saf_rel$temp)==FALSE),]
Saf_rel.child <- Saf_rel.child[,colSums(is.na(Saf_rel.child))<nrow(Saf_rel.child)]

Ind_rel.parent <- Ind_rel[which(is.na(Ind_rel$temp.3)==FALSE),]
Ind_rel.parent <- Ind_rel.parent[,colSums(is.na(Ind_rel.parent))<nrow(Ind_rel.parent)]
Ind_rel.child <- Ind_rel[which(is.na(Ind_rel$temp)==FALSE),]
Ind_rel.child <- Ind_rel.child[,colSums(is.na(Ind_rel.child))<nrow(Ind_rel.child)]

Res_rel.parent <- Res_rel[which(is.na(Res_rel$temp.3)==FALSE),]
Res_rel.parent <- Res_rel.parent[,colSums(is.na(Res_rel.parent))<nrow(Res_rel.parent)]
Res_rel.child <- Res_rel[which(is.na(Res_rel$temp)==FALSE),]
Res_rel.child <- Res_rel.child[,colSums(is.na(Res_rel.child))<nrow(Res_rel.child)]

cronbach(Frnd_rel.parent)
cronbach(Frnd_rel.child)
cronbach(Priv_rel.parent)
cronbach(Priv_rel.child)
cronbach(Saf_rel.parent)
cronbach(Saf_rel.child)
cronbach(Ind_rel.parent)
cronbach(Ind_rel.child)
cronbach(Res_rel.parent)
cronbach(Res_rel.child)

#alpha(Frnd_rel)
