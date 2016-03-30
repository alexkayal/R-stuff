library(reshape)
attitudeData<-read.delim("Attitude.dat", header = TRUE)
longAttitude <-melt(attitudeData, id = "participant", 
                    measured = c( "beerpos", "beerneg", "beerneut", 
                                  "winepos", "wineneg", "wineneut", 
                                  "waterpos", "waterneg", "waterneut"))
names(longAttitude)<-c("participant", "groups", "attitude")
longAttitude$drink<-gl(3, 60, labels = c("Beer", "Wine", "Water"))
longAttitude$imagery<-gl(3, 20, 180, labels = c("Positive", "Negative", "Neutral"))

baseline<-lme(attitude ~ 1, random = ~1|participant/drink/imagery, data = longAttitude, method = "ML")
drinkModel<-update(baseline, .~. + drink)
imageryModel<-update(drinkModel, .~. + imagery)
attitudeModel<-update(imageryModel, .~. + drink:imagery)
anova(baseline, drinkModel, imageryModel, attitudeModel)
summary(attitudeModel)
