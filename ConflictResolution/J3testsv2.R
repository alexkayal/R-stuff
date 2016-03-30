## lme models

## lme all cases

lme.all.baseline <- lme(userPref ~ 1, random = ~1|pp, data = prediction.data, 
                      correlation = corSymm(form = ~1|pp))
summary(lme.all.baseline)

lme.all.before <- lme(userPref ~ prediction.before, random = ~1|pp, data = prediction.data, 
                   correlation = corSymm(form = ~1|pp))
summary(lme.all.before)

## lme individual cases
currentData <- prediction.data
currentData$pp <- as.factor(currentData$pp)

lme.select.baseline <- lme(userPref01 ~ 1,
                         random = ~1|pp, data = currentData, 
                         correlation = corSymm(form = ~1|pp), method = "ML")
summary(lme.select.baseline)

lme.select.values.before <- lme(userPref01 ~ Frnd.before + Priv.before + Saf.before + Ind.before + Res.before,
                         random = ~1|pp, data = currentData, 
                         correlation = corSymm(form = ~1|pp), method = "ML")
summary(lme.select.values.before)

lme.select.values.avg <- lme(userPref01 ~ Frnd.avg + Priv.avg + Saf.avg + Ind.avg + Res.avg,
                                random = ~1|pp, data = currentData, 
                                correlation = corSymm(form = ~1|pp), method = "ML")
summary(lme.select.values.avg)

lme.select.values.consensus <- lme(userPref01 ~ Frnd.cons.before + Priv.cons.before + Saf.cons.before + Ind.cons.before + Res.cons.before,
                                random = ~1|pp, data = currentData, 
                                correlation = corSymm(form = ~1|pp), method = "ML")
summary(lme.select.values.consensus)

lme.select.normtype <- lme(userPref01 ~ norm.type,
                                random = ~1|pp, data = currentData, 
                                correlation = corSymm(form = ~1|pp), method = "ML")
summary(lme.select.normtype)

lme.select.normtype.values.before <- lme(userPref01 ~ norm.type + Frnd.before + Priv.before + Saf.before + Ind.before + Res.before,
                           random = ~1|pp, data = currentData, 
                           correlation = corSymm(form = ~1|pp), method = "ML")
summary(lme.select.normtype.values.before)

lme.select.normtype.values.avg <- lme(userPref01 ~ norm.type + Frnd.avg + Priv.avg + Saf.avg + Ind.avg + Res.avg,
                                         random = ~1|pp, data = currentData, 
                                         correlation = corSymm(form = ~1|pp), method = "ML")
summary(lme.select.normtype.values.avg)

lme.select.normtype.values.consensus <- lme(userPref01 ~ norm.type + Frnd.cons.before + Priv.cons.before + Saf.cons.before + Ind.cons.before + Res.cons.before,
                                         random = ~1|pp, data = currentData, 
                                         correlation = corSymm(form = ~1|pp), method = "ML")
summary(lme.select.normtype.values.consensus)

class <- data.frame(predicted = currentData$userPref01, response = round(predict(lme.select.baseline),0))
xtabs(~ predicted + response, data = class)

class <- data.frame(predicted = currentData$userPref01, response = round(predict(lme.select.values.before),0))
xtabs(~ predicted + response, data = class)

class <- data.frame(predicted = currentData$userPref01, response = round(predict(lme.select.values.avg),0))
xtabs(~ predicted + response, data = class)

class <- data.frame(predicted = currentData$userPref01, response = round(predict(lme.select.values.consensus),0))
xtabs(~ predicted + response, data = class)

class <- data.frame(predicted = currentData$userPref01, response = round(predict(lme.select.normtype),0))
xtabs(~ predicted + response, data = class)

class <- data.frame(predicted = currentData$userPref01, response = round(predict(lme.select.normtype.values.before),0))
xtabs(~ predicted + response, data = class)

class <- data.frame(predicted = currentData$userPref01, response = round(predict(lme.select.normtype.values.avg),0))
xtabs(~ predicted + response, data = class)

class <- data.frame(predicted = currentData$userPref01, response = round(predict(lme.select.normtype.values.consensus),0))
xtabs(~ predicted + response, data = class)

######

lme.select.age <- lme(userPref01 ~ age,
                           random = ~1|pp, data = currentData, 
                           correlation = corSymm(form = ~1|pp), method = "ML")
summary(lme.select.age)



lme.select.normtype <- lme(userPref01 ~ norm.type,
                         random = ~1|pp, data = currentData, 
                         correlation = corSymm(form = ~1|pp), method = "ML")
summary(lme.select.normtype)

lme.select.before.normtype <- lme(userPref01 ~ norm.type + Frnd.before + Priv.before + Saf.before + Ind.before + Res.before,,
                           random = ~1|pp, data = currentData, 
                           correlation = corSymm(form = ~1|pp), method = "ML")
summary(lme.select.before.normtype)

anova(lme.select.baseline, lme.select.role)

## gee models

## glm models

currentData = a715

logit0 <-glm(userPref01 ~ 1, data=currentData, family = "binomial")

logit1 <-glm(userPref01 ~ Frnd.cons.before+Priv.cons.before+Saf.cons.before+Ind.cons.before+Res.cons.before,
             data=currentData, family = "binomial")

logit2 <-glm(userPref01 ~ norm.type, 
             data=currentData, family = "binomial")

logit3 <-glm(userPref01 ~ norm.type + Frnd.cons.before+Priv.cons.before+Saf.cons.before+Ind.cons.before+Res.cons.before,
             data=currentData, family = "binomial")

class <- data.frame(predicted = currentData$userPref01, response = round(fitted(logit0),0))
xtabs(~ predicted + response, data = class)

class <- data.frame(predicted = currentData$userPref01, response = round(fitted(logit1),0))
xtabs(~ predicted + response, data = class)

class <- data.frame(predicted = currentData$userPref01, response = round(fitted(logit2),0))
xtabs(~ predicted + response, data = class)

class <- data.frame(predicted = currentData$userPref01, response = round(fitted(logit3),0))
xtabs(~ predicted + response, data = class)

summary(logit0)
summary(logit1)
summary(logit2)
summary(logit3)

anova(logit0,logit3)


