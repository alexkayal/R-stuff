fit <- aov(userPref1 ~ prediction.before1 +Error(pp/prediction.before1), data=prediction.data)
summary(fit)

sum =0
for (i in 1:nrow(prediction.data))
  if (prediction.data$userPref1[i]==prediction.data$prediction.before1[i])
    sum = sum +1
  

m1 <- ezANOVA(data = prediction.data, dv = userPref, 
              wid = pp, within = .(prediction), type = 3, detailed = TRUE)
m1$ANOVA

gee01 <- geeglm(userPref01 ~ prediction.before,
                id = pp, data = prediction.data, family=binomial, corstr="unstructured")

gee110 <- geeglm(userPref01 ~ Frnd.before + Priv.before + Saf.before + Ind.before + Res.before,
                id = pp, data = a110, family=binomial, corstr="unstructured")
summary(gee110)

summary(gee01)

gee02 <- geeglm(userPref ~ prediction * scope * trigger, id = pp, data = J3, family=gaussian, corstr="unstructured")
summary(gee02)

gee03 <- geeglm(userPref ~ prediction * scope * trigger, id = pp, data = J3, family=gaussian, corstr="unstructured")
summary(gee03)

gee04 <- geeglm(userPref ~ prediction, id = pp, data = J3, family=gaussian, corstr="unstructured")
summary(gee04)

lmer0 <-lmer(userPref ~ 1|pp, data = prediction.data, REML=FALSE)
lmer1 <-lmer(userPref ~ prediction + 1|pp, data = prediction.data, REML=FALSE)
lmer2 <-lmer(userPref ~ prediction + scope*trigger + 1|pp, data = prediction.data, REML=FALSE)

anova(lmer0,lmer1)

cs1 <- corAR1()
cs1 <- Initialize(cs1, data = J3)

lme0 <- lme(userPref01 ~ 1, random = ~1|pp, data = prediction.data, 
                   correlation = corSymm(form = ~1|pp), method="ML")
lme1.before <- lme(userPref01 ~ prediction.before, random = ~1|pp, data = prediction.data, 
                   correlation = corSymm(form = ~1|pp),method="ML")
lme1.after <- lme(userPref01 ~ prediction.after, random = ~1|pp, data = prediction.data, 
                   correlation = corSymm(form = ~1|pp),method="ML")

summary(lme0)
summary(lme1.before)
summary(lme1.after)
anova(lme0,lme1.before)

###

lme0.before <- lme(userPref01 ~ prediction.before, random = ~1|pp, data = prediction.data, 
                  correlation = corSymm(form = ~1|pp))
summary(lme0.before)

lme0.after <- lme(userPref ~ prediction.after, random = ~1|pp, data = prediction.data.hasKids.parent, 
                   correlation = corAR1(form = ~1|pp))
summary(lme0.after)

lme0.avg <- lme(userPref ~ prediction.avg, random = ~1|pp, data = prediction.data.hasKids.parent, 
                   correlation = corAR1(form = ~1|pp))
summary(lme0.avg)

lme.select.before <- lme(userPref ~ Frnd.before + Priv.before + Saf.before + Ind.before + Res.before,
                      random = ~1|pp, data = a110, 
                      correlation = corSymm(form = ~1|pp))
summary(lme.select.before)


lme1 <- lme(userPref ~ prediction.before*role*hasKids, random = ~1|pp, data = prediction.data, 
            correlation = corSymm(form = ~1|pp))
summary(lme1)

lme2 <- lme(VC1.familySecurity ~ C1.norm*C1.action*C1.thirdParty*C1.condition, random = ~1|role/pp, data = J3, 
            correlation = corAR1(form = ~1|role/pp))
summary(lme2)

t.test(J3$userPref, mu=0)

#lme0 <- update (lme0_basic, weights = varIdent(form = ~ 1|prediction), correlation = corAR1(form = ~1|pp))

glmm01<-glmm(userPref~prediction,random=list(~pp),
             varcomps.names=c("pp"), data=prediction.data,
             family.glmm=bernoulli.glmm,m=10^2,debug=TRUE,doPQL=FALSE)

logit0 <-glm(userPref01 ~ 1, data=prediction.data, family = "binomial")
logit1 <-glm(userPref01 ~ prediction.before, data=prediction.data, family = "binomial")
summary(logit1)
anova(logit0,logit1)


write.xlsx(J3, "/Users/alexkayal/Desktop/J3 fake data 50*2.xlsx")
