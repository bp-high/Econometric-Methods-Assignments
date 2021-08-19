library(PoEdata)
library(knitr)
library(xtable)
library(printr)
library(effects)
library(car)
library(AER)
library(broom) 
machine <- read.csv("~/R-Assignment_Econometric_methods/machine.data", header=FALSE)
names(machine) <- c("vendor", "model", "myct", "mmin", "mmax","cach", "chmin", "chmax", "prp", "erp")
View(machine)
s-tidy(machine)[,c(1:5,8,9)]
s-tidy(machine)
summary(machine,align="c")
mod1 <- lm(prp~myct+mmin+mmax+cach+chmin+chmax,data = machine)
smod1 <- summary(mod1)
smod1 <- data.frame(xtable(summary(mod1)))
kable(smod1, caption="The multiple regression model", col.names=c("coefficient", "Std. Error", "tvalue", "p-value"), align="c", digits=3)
smod1 <- summary(mod1)
Rsq <- smod1$r.squared
AdjRsq <- smod1$adj.r.squared
aic <- AIC(mod1)
bic <- BIC(mod1)
c(Rsq, AdjRsq, aic, bic)
p <- c(Rsq, AdjRsq, aic, bic)
kable(p)
p <- data.frame(c(Rsq, AdjRsq, aic, bic))
row.names(p)<-c("R-squared","Adjusted R-squared","AIC","BIC")
kable(p)
library(lmtest)
resettest(mod1, power=2, type="fitted")
resettest(mod1, power=2:3, type="fitted")
mod2 <- lm(prp~myct+mmax+cach,data = machine)
mod3 <- lm(prp~myct+mmax+cach+mmin,data = machine)
mod4 <- lm(prp~myct+mmax+cach+mmin+chmax,data = machine)
mod5 <- lm(prp~myct+mmax+cach+mmin+chmax+chmin,data = machine)
r1 <- as.numeric(glance(mod1))
r2 <- as.numeric(glance(mod2))
r3 <- as.numeric(glance(mod3))
r4 <- as.numeric(glance(mod4))
r5 <- as.numeric(glance(mod5))
tab <- data.frame(rbind(r1, r2, r3, r4,r5))[,c(1,2,8,9)]
row.names(tab) <- c("1","2","3","4","5")
kable(tab,
caption="Model comparison, 'machine' ", digits=15,
col.names=c("Rsq","AdjRsq","AIC","BIC"))
resettest(mod4, power=2:3, type="fitted")
mod2 <- lm(prp~mmax+cach,data = machine)
resettest(mod2, power=2:3, type="fitted")
mod2 <- lm(prp~mmax+myct,data = machine)
resettest(mod2, power=2:3, type="fitted")
mod2 <- lm(prp~mmax+mmin,data = machine)
resettest(mod2, power=2:3, type="fitted")
mod2 <- lm(prp~I(mmax^2)+I(mmin^2),data = machine)
resettest(mod2, power=2:3, type="fitted")
mod2 <- lm(prp~I(mmax^2)+I(mmin^2)+cach+myct+chmax+chmin,data = machine)
resettest(mod2, power=2:3, type="fitted")
resettest(mod2, power=2, type="fitted")
mod2
hyp<-c("4000*I(mmax^2)+1600*I(mmin^2)=250","(Intercept)+4000000*I(mmax^2)+640000*I(mmin^2)+myct+cach+chmin+chmax=20")
lhout <- tidy(linearHypothesis(mod2,hyp))
kable(lhout,
caption="Joint hypotheses with the 'linearHypothesis'
function")
kable(lhout,
caption="Joint hypotheses with the 'linearHypothesis'
function",digits=15)
kable(lhout,
caption="Joint hypotheses with the 'linearHypothesis'
function",digits=18)
lhout
linearHypothesis(mod2,hyp)
hyp<-c("4000*I(mmax^2)+1600*I(mmin^2)=250","(Intercept)+4000000*I(mmax^2)+640000*I(mmin^2)+myct+cach+chmin+chmax=20")
hyp<-c("4000*I(mmax^2)+1600*I(mmin^2)=250","(Intercept)+4000000*I(mmax^2)+640000*I(mmin^2)+800*myct+0*cach+0*chmin+0*chmax=20")
linearHypothesis(mod2,hyp)
lhout <- tidy(linearHypothesis(mod2,hyp))
kable(lhout,
caption="Joint hypotheses with the 'linearHypothesis'
function",digits=18)
colin<-tidy(vif(mod2))
kable(colin, caption="Variance inflation factors for the 'machine'
regression model", col.names=c("regressor","VIF"))
kable(tidy(bptest(mod2)),caption = "Breusch-Pagan heteroskedasticity test")
kable(tidy(bptest(mod2)),caption = "Breusch-Pagan heteroskedasticity test",digits=18)
res<-residuals(mod2)
yhat<-fitted(mod2)
plot(yhat,res, xlab="fitted values", ylab="residuals")
plot(yhat,res)
cov1 <- hccm(mod2, type="hc1")
mod2.HC1 <- coeftest(mod2, vcov.=cov1)
kable(tidy(mod2.HC1),caption=
"Robust (HC1) standard errors in the 'mod2/Machine' equation")
ehat<-resid(mod2)
ebar <- mean(ehat)
sde <- sd(ehat)
hist(ehat, col="grey", freq=FALSE, main="",
ylab="density", xlab="ehat")
curve(dnorm(x, ebar, sde), col=2, add=TRUE,
ylab="density", xlab="ehat")
library(tseries)
jarque.bera.test(ehat)
mod2
predpoint <- data.frame(myct=203 , mmin=2868 , mmax = 11800 , cach = 25 , chmin = 5 , chmax = 20 )
predict(mod2, newdata=predpoint,
interval="prediction")
predict(mod2, newdata=predpoint,interval="prediction")
temp<-predict(mod2, newdata=predpoint,interval="prediction")
kable(temp,caption="Forecasting in the properly adjusted 'Machine' model")
predict(mod2, newdata=predpoint,interval="confidence")
predict(mod2, newdata=predpoint)
predict(mod2, newdata=predpoint,interval)
predict(mod2, newdata=predpoint,interval="confidence")
temp <- predict(mod2, newdata=predpoint,interval="confidence")
kable(temp,caption="Predection for the estimated model")
kable(temp,caption="Prediction for the estimated model")


