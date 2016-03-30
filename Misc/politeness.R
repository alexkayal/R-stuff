require(lme4)
politeness= read.csv("http://www.bodowinter.com/tutorial/politeness_data.csv")
summary(politeness)
which(is.na(politeness$frequency))

boxplot(frequency ~ attitude*gender, col=c("white","lightgray"),politeness)

politeness.model = lmer(frequency ~ attitude + gender + (1|subject) +
                          (1|scenario), data=politeness, REML=FALSE)
politeness.null = lmer(frequency ~ gender +
                         (1|subject) + (1|scenario), data=politeness,
                          REML=FALSE)

anova(politeness.model,politeness.null)

summary(politeness.model)
