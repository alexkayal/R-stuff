#trying some Latent Class Analysis package on some public data

library(poLCA)

data(gss82)
f <- cbind(PURPOSE,ACCURACY,UNDERSTA,COOPERAT)~1
gss.lc2 <- poLCA(f,gss82,nclass=2) # log-likelihood = -2783.268
gss.lc3 <- poLCA(f,gss82,nclass=3,maxiter=3000,nrep=10) # log-likelihood = -2754.545
gss.lc4 <- poLCA(f,gss82,nclass=4,maxiter=15000,nrep=10,tol=1e-7) # log-likelihood = -2746.621

##
## Three models without covariates:
## M0: Loglinear independence model.
## M1: Two-class latent class model.
## M2: Three-class latent class model.
##
data(values)
f <- cbind(A,B,C,D)~1
M0 <- poLCA(f,values,nclass=1) # log-likelihood: -543.6498
M1 <- poLCA(f,values,nclass=2) # log-likelihood: -504.4677
M2 <- poLCA(f,values,nclass=3,maxiter=8000) # log-likelihood: -503.3011

##
## Three-class model with a single covariate.
##
data(election)
f2a <- cbind(MORALG,CARESG,KNOWG,LEADG,DISHONG,INTELG,
             MORALB,CARESB,KNOWB,LEADB,DISHONB,INTELB)~PARTY
nes2a <- poLCA(f2a,election,nclass=3,nrep=5)    # log-likelihood: -16222.32
pidmat <- cbind(1,c(1:7))
exb <- exp(pidmat %*% nes2a$coeff)
matplot(c(1:7),(cbind(1,exb)/(1+rowSums(exb))),ylim=c(0,1),type="l",
        main="Party ID as a predictor of candidate affinity class",
        xlab="Party ID: strong Democratic (1) to strong Republican (7)",
        ylab="Probability of latent class membership",lwd=2,col=1)
text(5.9,0.35,"Other")
text(5.4,0.7,"Bush affinity")
text(1.8,0.6,"Gore affinity")


##
ds = read.csv("http://www.math.smith.edu/r/data/help.csv")
ds = within(ds, (cesdcut = ifelse(cesd>20, 1, 0)))
res2 = poLCA(cbind(homeless=homeless+1, 
                   cesdcut=cesdcut+1, satreat=satreat+1, 
                   linkstatus=linkstatus+1) ~ 1, 
             maxiter=50000, nclass=3, 
             nrep=10, data=ds)