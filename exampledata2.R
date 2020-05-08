mydata <- read.csv("https://stats.idre.ucla.edu/stat/data/binary.csv")
mydata
names(mydata)
dim(mydata)
summary(mydata)
pairs(mydata)
attach(mydata)
sapply(mydata,sd)
xtabs(~admit + rank, data = mydata)
par(mfrow=c(1,1))
plot(mydata$gre,mydata$rank)

#linear regression
fit<-lm(admit~gre, data=mydata)
summary(fit)
p<-c(rep(0,400))
for(i in 1:400){
  p[i]<--0.1198407+0.0007442*mydata$gre[i]
}
head(p)
plot(mydata$gre,p)
abline(-0.1198407,0.0007442)

#figure 1&table1
glm.fit<-glm(admit~gre,data=mydata,family="binomial")
summary(glm.fit)
curve(expr=exp(-2.901344+0.003582*x)/(1+exp(-2.901344+0.003582*x)),xlim=c(0,2000),col="black",xlab=expression(x),ylab=expression(p))
glm.probs<-predict(glm.fit,type="response")
glm.pred<-ifelse(glm.probs>0.5,"1","0")
table(glm.pred,mydata$admit)
mean(glm.pred==mydata$admit)


#figure 2
par(mfrow=c(1,3))
curve(expr=exp(0.1-0.3*x)/(1+exp(0.1-0.3*x)),xlim=c(-20,20),col="black",xlab=expression(x),ylab=expression(p))
curve(expr=exp(0.1+0.3*x)/(1+exp(0.1+0.3*x)),xlim=c(-20,20),col="black",xlab=expression(x),ylab=expression(p))
curve(expr=exp(0.1+0.8*x)/(1+exp(0.1+0.8*x)),xlim=c(-20,20),col="black",xlab=expression(x),ylab=expression(p))

#table2
mydata$rank<- factor(mydata$rank)
glm.fit<-glm(admit~rank,data=mydata,family="binomial")
summary(glm.fit)
glm.probs<-predict(glm.fit,type="response")
glm.pred<-ifelse(glm.probs>0.5,"1","0")
table(glm.pred,mydata$admit)
mean(glm.pred==mydata$admit)

#multi predictors logistic regression
mydata$rank<- factor(mydata$rank)
glm.fit<-glm(admit~gre+gpa+rank,data=mydata,family="binomial")
summary(glm.fit)
glm.probs<-predict(glm.fit,type="response")
glm.pred<-ifelse(glm.probs>0.5,"1","0")
table(glm.pred,mydata$admit)
mean(glm.pred==mydata$admit)

#cI for parameter
fit2<-glm(admit~gre,data=mydata,family=binomial(link=logit))
confint(fit2)

#wald test
library(aod)
wald.test(Sigma=vcov(glm.fit),b=coef(glm.fit) ,Terms = 2)

#likelihood ratio test
require(zoo)
require(lmtest)
fit1<-glm(admit~1,data=mydata,family=binomial(link=logit))
fit2<-glm(admit~gre,data=mydata,family=binomial(link=logit))
lrtest(fit2,fit1)

#distribution of probability estimates
install.packages("ggplot2")
library(ggplot2)
x1<-with(mydata,data.frame(gre=seq(min(gre),max(gre),0.1)))
pred<-predict(fit2,newdata=x1,type="link",se.fit=TRUE)
upr<-pred$fit+(1.96*pred$se.fit)
lwr<-pred$fit-(1.96*pred$se.fit)
tail(pred$se.fit)
fit<-pred$fit
fit3 <- fit2$family$linkinv(fit)
upr3 <- fit2$family$linkinv(upr)
lwr3 <- fit2$family$linkinv(lwr)
x1$lwr <- lwr3 
x1$upr <- upr3 
ggplot(data=mydata, mapping=aes(x=gre,y=admit)) + geom_point() +         
  stat_smooth(method="glm", method.args=list(family=binomial)) + 
  geom_line(data=x1, mapping=aes(x=gre, y=upr), col="red") + 
  geom_line(data=x1, mapping=aes(x=gre, y=lwr), col="red") 

#model checking continuous predictor

dim(xtabs(~admit + gpa, data = mydata))
g1<-c(2.26, 2.42, 2.48)
g2<-c(2.52, 2.55, 2.56, 2.62, 2.63, 2.65, 2.67,2.68 ,2.69, 2.7, 2.71, 2.73)
g3<-c( 2.76, 2.78, 2.79, 2.81 ,2.82,2.83 ,2.84, 2.85, 2.86 ,2.87, 2.88 ,2.9 ,2.91, 2.92, 2.93, 2.94, 2.95 ,2.96 ,2.97, 2.98)
g4<-c(3, 3.01, 3.02, 3.03, 3.04, 3.05, 3.06, 3.07 ,3.08,3.09, 3.1, 3.11 ,3.12 ,3.13 ,3.14,3.15, 3.16, 3.17, 3.18, 3.19, 3.2, 3.21, 3.22, 3.23, 3.24)
g5<-c(3.25,3.27, 3.28, 3.29, 3.3, 3.31, 3.32, 3.33, 3.34, 3.35,3.36, 3.37, 3.38 ,3.39 ,3.4 ,3.41, 3.42 ,3.43 ,3.44, 3.45,3.46,3.47,3.48,3.49)
g6<-c(3.5 ,3.51, 3.52, 3.53, 3.54, 3.55, 3.56, 3.57, 3.58, 3.59 ,3.6, 3.61 ,3.62, 3.63, 3.64, 3.65,3.66 ,3.67,3.69,3.7,3.71,3.72,3.73,3.74)
g7<-c(3.75, 3.76,3.77 ,3.78, 3.8, 3.81 ,3.82, 3.83, 3.84, 3.85, 3.86, 3.87,3.88, 3.89 ,3.9 ,3.91, 3.92, 3.93, 3.94, 3.95, 3.97, 3.98,3.99)
c1<-c(1,2,1)
c2<-c(1,1,1,2,1,1,2,1,1,2,2,1)
c3<-c(1,2,2,3,2,1,1,2,2,1,2,4,3,2,5,3,1,2,2,6)
c4<-c(4,2,4,1,2,3,1,4,4,1,1,1,4,5,4,7,2,5,1,5,2,1,5,3,2)
c5<-c(2,3,4,2,4,8,4,5,5,7,4,3,5,3,7,1,1,5,3,7,5,3,3,5)
c6<-c(4,5,4,2,3,1,3,3,5,5,3,3,2,6,5,4,1,4,3,3,2,1,2,4)
c7<-c(2,2,5,4,2,3,1,1,2,1,2,1,3,3,3,1,2,1,5,5,1,1,3)
try2<-glm(admit~gpa,data=mydata,family=binomial(link=logit))
try2
g1<-exp(-4.358+1.051*g1)/(1+exp(-4.358+1.051*g1))
g1%*%c1
observed<-c(1,4,10,20,24,35,20,13,3,12,37,54,75,43,34,15)
fitted<-c(0.55,2.75,9.95,18.87,30.51,28.31,23.08,12.92)
fitted<-c(fitted,n-fitted)
fitted
X2=sum((observed-fitted)^2/fitted)
X2
G2=2*sum(observed*log(observed/fitted))
G2

mean<-c(g1%*%c1/sum(c1),g2%*%c2/sum(c2),g3%*%c3/sum(c3),g4%*%c4/sum(c4),g5%*%c5/sum(c5),g6%*%c6/sum(c6),g7%*%c7/sum(c7),4)
mean
yes<-c(1,4,10,20,24,35,20,13)
n<-c(4,16,47,74,99,78,54,28)
try<-glm(yes/n~mean,weight=n,family=binomial(link=logit))
try
fitted<-exp(-3.8657+0.9084*mean)/(1+exp(-3.8657+0.9084*mean))*n
fitted
fitted<-c(fitted,n-fitted)
fitted
X2=sum((observed-fitted)^2/fitted)
X2
G2<-2*sum(observed*log(observed/fitted))
G2


#model checking for qualitative factor
mydata$rank<- factor(mydata$rank)
glm.fit1<-glm(admit~rank,data=mydata,family=binomial(link=logit))
glm.fit2<-glm(admit~1,data=mydata,family=binomial(link=logit))
lrtest(glm.fit1,glm.fit2)
data<-matrix(c(33,28,54,97,28,93,12,55),ncol=2,byrow=TRUE)
chisq.test(data)
mydata$rank<- as.numeric(mydata$rank)
linearlogit<-glm(admit~rank,data=mydata,family=binomial(link=logit))
lrtest(linearlogit,glm.fit2)

lm1<-glm(admit~rank+gre+gpa,data=mydata,family=binomial(link=logit))
lm2<-glm(admit~rank+gre,data=mydata,family=binomial(link=logit))
summary(lm1)
lrtest(lm1,lm2)



#model comparison test
summary(lm1)
lm3<-glm(admit~gpa+gre,data=mydata,family=binomial(link=logit))
summary(lm3)
lm4<-glm(admit~gpa+gre+rank+rank*gpa,data=mydata,family=binomial(link=logit))
summary(lm4)
lrtest(lm1,lm4)

#quantitative treatment of ordinal predictor
mydata$rank<- as.numeric(mydata$rank)
lm5<-glm(admit~gpa+gre+rank,data=mydata,family="binomial")
summary(lm5)

rank.score2<-c(2,2,0,2,2,1,0,1,2,1,2,0,0,1,0,2,2,2,1,0,2,
          1,2,2,1,0,0,2,1,0,2,2,2,2,0,1,0,2,1,2,1,1,
         1,2,1,2,1,2,2,2,2,2,2,1,2,2,2,2,1,2,1,2,2,
         2,2,1,2,0,0,0,2,2,2,1,2,2,2,2,0,0,2,1,1,2,
          2,1,1,1,0,1,1,0,1,1,1,1,2,1,1,2,2,2,2,2,1,
         1,0,1,2,1,2,2,2,0,2,2,1,1,0,2,1,1,2,2,2,2,
         0,2,1,2,1,1,1,2,1,2,2,2,1,0,1,2,2,2,2,2,1,
          2,0,0,0,1,1,2,2,2,1,0,1,2,1,1,1,1,1,0,2,2,
         2,2,2,2,2,1,2,1,1,2,2,2,2,2,1,1,2,1,2,1,1,
         1,1,2,2,2,1,1,2,2,2,2,2,1,0,2,0,2,0,0,2,1,
         2,1,1,2,1,2,0,0,0,1,2,2,0,2,1,2,1,2,1,1,2,
         2,1,2,0,1,1,1,2,2,1,0,2,1,0,2,1,1,2,2,2,2,
         1,2,2,2,1,2,1,1,1,1,2,2,2,2,2,2,1,2,1,2,1,
         0,1,1,2,0,2,1,1,2,2,2,1,2,0,2,2,2,1,1,1,0,
         0,2,0,1,1,2,1,2,1,1,2,2,0,1,1,2,2,1,2,2,2,
         1,1,2,2,0,2,1,2,1,2,0,1,1,1,2,2,2,0,2,2,0,
         2,2,0,2,2,2,2,1,2,2,1,1,1,1,1,2,2,1,1,0,1,
         0,2,2,0,0,1,1,0,2,2,2,0,1,1,2,0,0,1,2,1,1,
         2,1,1,1,1,0,1,0,1,1,1,1,1,1,2,1,2,1,2,1,1,2)
lm6<-glm(admit~gpa+gre+rank.score2,data=mydata,family="binomial")
summary(lm6)
lrtest(lm5,lm1)
lrtest(lm6,lm1)
lrtest(lm6,lm5)
lrtest(lm1,glm.fit2)


#model selection 
cor(mydata$rank,mydata$gpa)
cor(mydata$rank,mydata$gre)
cor(mydata$gre,mydata$gpa)
cor(mydata$admit,mydata$gre)
cor(mydata$admit,mydata$gpa)
cor(mydata$admit,mydata$rank)
model1<-glm(admit~gre+gpa+rank+rank*gpa+rank*gre+gre*gpa+gpa*gre*rank, data=mydata,family = "binomial")
summary(model1)
model2<-glm(admit~gre+gpa+rank+rank*gpa+rank*gre+gre*gpa, data=mydata,family = "binomial")
summary(model3a)
model3a<-glm(admit~gre+gpa+rank+rank*gre+gre*gpa, data=mydata,family = "binomial")
model3b<-glm(admit~gre+gpa+rank+rank*gpa+gre*gpa, data=mydata,family = "binomial")
model3c<-glm(admit~gre+gpa+rank+rank*gpa+rank*gre, data=mydata,family = "binomial")
model4a<-glm(admit~gre+gpa+rank+gre*gpa, data=mydata,family = "binomial")
model4b<-glm(admit~gre+gpa+rank+rank*gre, data=mydata,family = "binomial")
model5<-glm(admit~gre+gpa+rank, data=mydata,family = "binomial")
model6c<-glm(admit~gre+gpa, data=mydata,family = "binomial")
model6b<-glm(admit~gre+rank, data=mydata,family = "binomial")
model6a<-glm(admit~gpa+rank, data=mydata,family = "binomial")
model7b<-glm(admit~gpa, data=mydata,family = "binomial")
model7a<-glm(admit~rank, data=mydata,family = "binomial")
model7c<-glm(admit~gre, data=mydata,family = "binomial")
model8a<-glm(admit~gpa+rank.score2, data=mydata,family = "binomial")
model8b<-glm(admit~gre+rank.score2, data=mydata,family = "binomial")
model9<-glm(admit~1, data=mydata,family = "binomial")
summary(model7b)
lrtest(model5,model8a)
step(model1,direction="backward")
BIC(model8a)


#residual diagnostic
par(mfrow=c(1,1))
r<-rstandard(model7b,type="pearson")
plot(mydata$gpa,r,bty="l",cex=0.75,cex.lab=0.75,
     ylab="Standardized Residuals",
     xlab="GPA")
abline(0,0)
identify(mydata$gpa,r)
r[373]
mydata[373,]
mean<-c(g1%*%c1/sum(c1),g2%*%c2/sum(c2),g3%*%c3/sum(c3),g4%*%c4/sum(c4),g5%*%c5/sum(c5),g6%*%c6/sum(c6),g7%*%c7/sum(c7),4)
mean
yes<-c(1,4,10,20,24,35,20,13)
n<-c(4,16,47,74,99,78,54,28)
yes/n
sum(yes)/sum(n)
try<-glm(yes/n~mean,weight=n,family=binomial(link=logit))
summary(try)
p1<-exp(-3.8657+0.9084*mean)/(1+exp(-3.8657+0.9084*mean))
fitted1<-n*p1
fitted1
stresidual1<-rstandard(try,type="pearson")
stresidual1
pearson1<-sum(stresidual1^2)
pearson1
try2<-glm(yes/n~1,weight=n,family=binomial(link=logit))
try2
p2<-exp(-0.7653)/(1+exp(-0.7653))
p2
fitted2<-n*p2
fitted2
stresidual2<-rstandard(try2,type="pearson")
stresidual2
pearson2<-sum(stresidual2^2)
pearson2

plot(mean,yes/n,ylab="Proportion",xlab="GPA",ylim=c(0,0.5),pch=4,bty="l")
xlim<-range(mydata$gpa)
f1<-function(x) exp(-3.8657+0.9084*x)/(1+exp(-3.8657+0.9084*x))
curve(f1,add=TRUE,xlim=xlim,ylim=c(0,0.45),col="black")

influence.measures(try)
influence.measures(try2)
d1<-glm(yes[-1]/n[-1]~mean[-1],weight=n[-1],family=binomial(link=logit))
summary(d1)
confint(d1)
confint(try)
pear1<-sum(rstandard(d1,type="pearson")^2)
pear1
d2<-glm(yes[-2]/n[-2]~mean[-2],weight=n[-2],family=binomial(link=logit))
summary(d2)
confint(d2)
pear2<-sum(rstandard(d2,type="pearson")^2)
pear2
d3<-glm(yes[-3]/n[-3]~mean[-3],weight=n[-3],family=binomial(link=logit))
summary(d3)
confint(d3)
pear3<-sum(rstandard(d3,type="pearson")^2)
pear3
d4<-glm(yes[-4]/n[-4]~mean[-4],weight=n[-4],family=binomial(link=logit))
summary(d4)
confint(d4)
pear4<-sum(rstandard(d4,type="pearson")^2)
pearson1-pear4
d5<-glm(yes[-5]/n[-5]~mean[-5],weight=n[-5],family=binomial(link=logit))
summary(d5)
confint(d5)
pear5<-sum(rstandard(d5,type="pearson")^2)
pearson1-pear5
d6<-glm(yes[-6]/n[-6]~mean[-6],weight=n[-6],family=binomial(link=logit))
summary(d6)
confint(d6)
pear6<-sum(rstandard(d6,type="pearson")^2)
pearson1-pear6
d7<-glm(yes[-7]/n[-7]~mean[-7],weight=n[-7],family=binomial(link=logit))
summary(d7)
confint(d7)
pear7<-sum(rstandard(d7,type="pearson")^2)
pearson1-pear7
d8<-glm(yes[-8]/n[-8]~mean[-8],weight=n[-8],family=binomial(link=logit))
summary(d8)
confint(d8)
pear8<-sum(rstandard(d8,type="pearson")^2)
pearson1-pear8


summary(try2)
d1<-glm(yes[-1]/n[-1]~1,weight=n[-1],family=binomial(link=logit))
summary(d1)
pear1<-sum(rstandard(d1,type="pearson")^2)
pearson2-pear1
d2<-glm(yes[-2]/n[-2]~1,weight=n[-2],family=binomial(link=logit))
summary(d2)
pear2<-sum(rstandard(d2,type="pearson")^2)
pearson2-pear2
d3<-glm(yes[-3]/n[-3]~1,weight=n[-3],family=binomial(link=logit))
summary(d3)
pear3<-sum(rstandard(d3,type="pearson")^2)
pearson2-pear3
d4<-glm(yes[-4]/n[-4]~1,weight=n[-4],family=binomial(link=logit))
summary(d4)
pear4<-sum(rstandard(d4,type="pearson")^2)
pearson2-pear4
d5<-glm(yes[-5]/n[-5]~1,weight=n[-5],family=binomial(link=logit))
summary(d5)
pear5<-sum(rstandard(d5,type="pearson")^2)
pearson2-pear5
d6<-glm(yes[-6]/n[-6]~1,weight=n[-6],family=binomial(link=logit))
summary(d6)
pear6<-sum(rstandard(d6,type="pearson")^2)
pearson2-pear6
d7<-glm(yes[-7]/n[-7]~1,weight=n[-7],family=binomial(link=logit))
summary(d7)
pear7<-sum(rstandard(d7,type="pearson")^2)
pearson2-pear7
d8<-glm(yes[-8]/n[-8]~1,weight=n[-8],family=binomial(link=logit))
summary(d8)
pear8<-sum(rstandard(d8,type="pearson")^2)
pearson2-pear8

#summary of predictive power
prob<-predict(model1,type="response")
cor(mydata$admit,prob)
prob<-predict(model2,type="response")
cor(mydata$admit,prob)
prob<-predict(model3a,type="response")
cor(mydata$admit,prob)
prob<-predict(model3b,type="response")
cor(mydata$admit,prob)
prob<-predict(model3c,type="response")
cor(mydata$admit,prob)
prob<-predict(model4a,type="response")
cor(mydata$admit,prob)
prob<-predict(model4b,type="response")
cor(mydata$admit,prob)
prob<-predict(model5,type="response")
cor(mydata$admit,prob)
prob<-predict(model6a,type="response")
cor(mydata$admit,prob)
prob<-predict(model6b,type="response")
cor(mydata$admit,prob)
prob<-predict(model6c,type="response")
cor(mydata$admit,prob)
prob<-predict(model7a,type="response")
cor(mydata$admit,prob)
prob<-predict(model7b,type="response")
cor(mydata$admit,prob)
prob<-predict(model7c,type="response")
cor(mydata$admit,prob)
prob<-predict(model8a,type="response")
cor(mydata$admit,prob)
prob<-predict(model8b,type="response")
cor(mydata$admit,prob)
prob<-predict(model9,type="response")
cor(mydata$admit,prob)
lrtest(model9,model8a)
install.packages("rcompanion")
library(rcompanion)
anova(model1,model2,model3a,model3b,model3c,model4a,model4b,model5,model6a,model6b,model6c,model7a,model7b,model8a,model9)

#classification table
mydata$rank<- factor(mydata$rank)
model5<-glm(admit~gre+gpa+rank, data=mydata,family = "binomial")
prob<-predict(model5,data=mydata,type="response")
pred<-ifelse(probs > 0.5, "1","0")
Admit=mydata$admit
table(pred,Admit)
mean(pred==Admit)


#chapter3 knn
#the choice of K 
library(class)
set.seed(100)
n1<-100
y1<-rep(0,n1)
y2<-rep(1,n1)
x11 <- rnorm(n1,280,10)
x12 <- rnorm(n1,5,1)
x1 <- cbind(x11,x12)
x21 <- rnorm(n1,300,10)
x22 <- rnorm(n1,7,1)
x2 <- cbind(x21,x22)
x<-rbind(x1,x2)
standardx<-scale(x)
summary(standardx)
data <- data.frame("obs1"=c(standardx[,1]),"obs2"=c(standardx[,2]),category=as.factor(c(y1,y2)))
y <- data$category
x<-cbind(data$obs1,data$obs2)
px1 <- seq(-3.4,3.4,length.out=79)
px2 <- seq(-3.4,3.4,length.out=99)
test <- matrix(c(rep(px1,length.out=length(px1)*length(px2)),rep(px2,each=length(px1))),byrow=FALSE,ncol=2)
test<-data.frame("testx1"=c(test[,1]),"testx2"=c(test[,2]))
mod <- knn(standardx, test, y, k=1, prob=TRUE)
prob <- attr(mod, "prob")
prob <- ifelse(mod=="1", prob, 1-prob)
probmod <- matrix(prob, length(px1), length(px2))
par(mfrow=c(1,2),mar=c(2,0,2,0),oma=c(rep(0.05,4)))
contour(px1,px2, probmod, levels=0.5, labels="", xlab="", ylab="", main="1-nearest neighbour", axes=FALSE)
points(standardx, col=ifelse(y==1, "chocolate1", "chartreuse3"))
lines(c(3.4,-3.4),c(-3.4,3.4),pch=5,col="cyan3")
gd <- expand.grid(x=px1, y=px2)
points(gd, pch=".", cex=1.2, col=ifelse(probmod>0.5, "chocolate1", "chartreuse3"))
box()
mod <- knn(standardx, test, y, k=15, prob=TRUE)
prob <- attr(mod, "prob")
prob <- ifelse(mod=="1", prob, 1-prob)
probmod <- matrix(prob, length(px1), length(px2))
contour(px1, px2, probmod, levels=0.5, labels="", xlab="", ylab="", main=
          "15-nearest neighbours", axes=FALSE)
points(standardx, col=ifelse(y==1, "chocolate1", "chartreuse3"))
lines(c(3.4,-3.4),c(-3.4,3.4),pch=5,col="cyan3")
gd <- expand.grid(x=px1, y=px2)
points(gd, pch=".", cex=1.2, col=ifelse(probmod>0.5, "chocolate1", "chartreuse3"))
box()

library(e1071)
library(mlr)
task1 = makeClassifTask(id = "simulation1", data = data, target = "category")
lrn_knn_15 = makeLearner("classif.knn",k=15)
lrn_knn_1 = makeLearner("classif.knn",k=1)
model_knn_15 = train(learner = lrn_knn_15, task = task1)
model_knn_1 = train(learner=lrn_knn_1, task=task1)
data.pred_knn_15 = predict(object=model_knn_15,task1)
data.pred_knn_1 = predict(object=model_knn_1,task1)
calculateConfusionMatrix(data.pred_knn_15)
calculateConfusionMatrix(data.pred_knn_1)

set.seed(98)
n2 <- 10300
y1<-rep(0,n2)
y2<-rep(1,n2)
x11 <- rnorm(n2,280,10)
x12 <- rnorm(n2,5,1)
x1 <- cbind(x11,x12)
x21 <- rnorm(n2,300,10)
x22 <- rnorm(n2,7,1)
x2 <- cbind(x21,x22)
x<-rbind(x1,x2)
standardx<-scale(x)
data2 <- data.frame("obs1"=c(standardx[,1]),"obs2"=c(standardx[,2]),"category"=c(as.factor(c(y1,y2))))
y<-data2$category
set.seed(95)
t200=sample(20600,200,replace=F)
train200=data2[t200,]
t200=sample(20600,200,replace=F)
test200=data2[t200,]
t1000=sample(20600,1000,replace=F)
test1000=data2[t1000,]
t20000=sample(20600,20000,replace=F)
test20000=data2[t20000,]

K <- c(1:100)
m <- length(K)
task.train = makeClassifTask(id = "simulation", data = train200, target = "category")
task.test.big = makeClassifTask(id = "simulation2", data = test1000, target = "category")
task.test.small = makeClassifTask(id = "pissoff", data = test200, target = "category")
task.true = makeClassifTask(id = "trueerror", data=test20000, target="category")
ho = makeResampleInstance("Holdout",task.train)
task.train.better = subsetTask(task.train,ho$train.inds[[1]])
task.test.better = subsetTask(task.train,ho$test.inds[[1]])
Err.test.small <- matrix(0,nrow=m,ncol=10)
Err.test.big <- matrix(0,nrow=m,ncol=10)
Err.train <- matrix(0,nrow=m,ncol=10)
Err.true <- matrix(0,nrow=m,ncol=1)
for(i in 1:m){ 
  for(j in 1:10){
    lrn = makeLearner("classif.knn",k=i)
    model = train(learner = lrn, task = task.train)
    data.pred = predict(object=model,task.train)
    confmat <- calculateConfusionMatrix(data.pred)
    Err.train[i,j] <- confmat$result[3,3]/(200)
  }
}
for(i in 1:m){ 
  for(j in 1:10){
    lrn = makeLearner("classif.knn",k=i)
    model = train(learner = lrn, task = task.train)
    data.pred = predict(object=model,task.test.big)
    confmat <- calculateConfusionMatrix(data.pred)
    Err.test.big[i,j] <- confmat$result[3,3]/(1000)
  }
}
for(i in 1:m){ 
  for(j in 1:10){
    lrn = makeLearner("classif.knn",k=i)
    model = train(learner = lrn, task = task.train)
    data.pred = predict(object=model,task.test.small)
    confmat <- calculateConfusionMatrix(data.pred)
    Err.test.small[i,j] <- confmat$result[3,3]/(200)
    }
}

for(i in 1:m){ 
    lrn = makeLearner("classif.knn",k=i)
    model = train(learner = lrn, task = task.train)
    data.pred = predict(object=model,task.true)
    confmat <- calculateConfusionMatrix(data.pred)
    Err.true[i,] <- confmat$result[3,3]/(20000)
}

par(mar=c(rep(4,4)),mfrow=c(1,1))
err.train <- c(rowMeans(Err.train))
err.test.big <- c(rowMeans(Err.test.big))
err.test.small <- c(rowMeans(Err.test.small))
Error <- structure(c(err.test.big,err.test.small,err.train), dim=c(m,3), dimnames=list(K,c("Test Error (large test set)","Test Error (small test set)","Trainng Error")))
matplot(rownames(Error),Error, type='l', xlab="K",ylab="Error",main="Errors For Varying K",col=c(3,5,7),lwd=c(2,2,2))
legend('bottomright', inset=.05, legend=c("True Error","Test (big)","Test (small)","Training"), pch=c("_","_","_","_"), horiz=TRUE, col=c(1,3,5,7), cex = 0.75,pt.cex=0.75)
which.min(Err.true)   #27
abline(a=Err.true[27],b=0)




#admit data 
par(mfrow=c(1,1))
install.packages("caret")
install.packages("e1071")
library(e1071)
require(caret)
mydata$rank<- as.numeric(mydata$rank)
head(mydata)
standardized.X=scale(mydata[,-1])
head(standardized.X)
mydata$admit<-as.factor(mydata$admit)
standardized.mydata<-data.frame("gre"=c(standardized.X[,1]),"gpa"=c(standardized.X[,2]),"rank"=c(standardized.X[,3]),"admit"=c(mydata$admit))
head(standardized.mydata)
standardized.mydata$admit<-as.factor(standardized.mydata$admit)
trainControl<-trainControl(method="cv",number =10)
knnfit<-train(admit~.,
           method  ="knn",
           tuneGrid=expand.grid(k=1:10),
           trControl=trControl,
           metric="Accuracy",
           data    = standardized.mydata)
knnfit
library(class)
set.seed(1000)
t=sample(400,320,replace=F)
train10=standardized.mydata[t,]
test10=standardized.mydata[-t,]

#k=1
set.seed(1001)
knn.pred=knn(train10[,-4],train10[,-4],train10$admit,k=1)
table(knn.pred,train10$admit)
mean(knn.pred==train10$admit)
knn.pred=knn(train10[,-4],test10[,-4],train10$admit,k=1)
table(knn.pred,test10$admit)
mean(knn.pred==test10$admit)
41#k=9
knn.pred=knn(train10[,-4],train10[,-4],train10$admit,k=9)
table(knn.pred,train10$admit)
mean(knn.pred==train10$admit)
knn.pred=knn(train10[,-4],test10[,-4],train10$admit,k=9)
table(knn.pred,test10$admit)
mean(knn.pred==test10$admit)
#k=99

knn.pred=knn(train10[,-4],train10[,-4],train10$admit,k=99)
table(knn.pred,train10$admit)
mean(knn.pred==train10$admit)
knn.pred=knn(train10[,-4],test10[,-4],train10$admit,k=99)
table(knn.pred,test10$admit)
mean(knn.pred==test10$admit)
summary(mydata)



#imputation
#listwise deletion
set.seed(98)
tmiss1=sample(400,10,replace=F)
tmiss2=sample(400,3,replace = F)  
tmiss3=sample(400,15,replace = F)
missingdata1<-c(380, 660, 800, 640, 520, 760, 560, 400, 540, 700,
                 800, NA, 760, 700, 700, 480, 780, 360, 800, 540,
                 500, 660, 600, 680, 760, 800, 620, 520, 780, 520,
                 540, 760, 600, 800, 360, 400, 580, 520, 500, 520,
                 560, 580, 600, 500, 700, 460, 580, 500, 440, 400,
                 640, 440, 740, 680, 660, 740, 560, 380, 400, 600,
                 620, 560, 640, 680, 580, 600, 740, 620, 580, 800,
                 640, 300, 480, 580, 720, 720, 560, 800, 540, 620,
                 700, 620, 500, 380, 500, 520, NA, 600, 700, 660,
                 700, 720, 800, 580, 660, 660, 640, 480, NA , 400,
                 340, 580, 380, 540, 660, 740, 700, 480, 400, 480,
                 680, 420, 360, 600, 720, 620, 440, 700, 800, 340,
                 520, 480, NA , 500, 720, 540, 600, 740, 540, 460,
                 620, NA  ,580, 500, 560, 500, 560, NA,  620, 600,
                 640, 700, 620, 580, 580, 380, 480, 560, 480, 740,
                 800, 400, 640, 580, 620, 580, 560, 480, NA, 700,
                 600, 640, 700, 520, 580, 700, 440, 720, 500, 600,
                 400 ,540, 680, 800, 500, 620, 520 ,620, 620, 300,
                 620, 500, 700, 540, 500, 800, NA , 580, NA , 500,
                 640, 800, 640, 380, 600, 560, 660, 400, 600, 580,
                 800, 580, 700, 420, 600, 780, 740, 640, 540, 580,
                 740, 580, 460, 640, 600, 660, 340, 460, 460, 560,
                 540, 680, 480, 800, 800, 720, 620, 540, 480, 720,
                 580, 600, 380, 420, 800, 620, 660, 480, 500, 700,
                 440, 520, 680, 620, 540, 800, 680, 440, 680, 640,
                 660, 620, 520, 540, 740, 640, 520, 620, 520, 640,
                 680, 440, 520, 620, 520, 380, 560, 600, 680, 500,
                 640, 540, 680, 660, 520, 600, 460, 580, 680, 660,
                 660, 360, NA,  520, 440, 600, 800, 660, 800 ,420,
                 620, 800, 680, 800, 480, 520, 560, 460, 540, 720,
                 640, 660, 400, 680, 220, 580, 540, 580, 540, 440,
                 560, 660, 660, 520, 540, 300, 340, 780, 480, 540,
                 460, 460, 500, 420, 520, 680, 680, 560, 580, 500,
                 740, 660, 420, 560, 460, 620, 520, 620, 540, 660,
                 500, 560, 500, 580, 520, 500, 600, 580, 400, 620,
                 780, 620, 580, 700, 540, 760, 700, 720, 560, 720,
                 520, 540, 680, 460, 560, 480, 460, 620, 580, 800,
                 540, 680, 680, 620, 560, 560, 620, 800, 640, 540,
                 700, 540, 540, 660, 480, 420, 740, 580, 640, 640,
                 800, 660, 600, 620, 460, 620, 560, 460, 700, 600)
missingdata2<-mydata$gpa
missingdata2[tmiss2]<-rep(NA,3)
missingdata3<-mydata$rank
missingdata3[tmiss3]<-rep(NA,15)
missingdata<-data.frame("admit"=c(mydata$admit),"gre"=c(missingdata1),"gpa"=c(missingdata2),"rank"=c(missingdata3))
completedata<-na.omit(missingdata)
install.packages("mice")
library(mice)
md.pattern(missingdata)
dim(completedata)
exam1<-mydata[1:4,1:4]
exam1[3,2:3]<-rep(NA,2)
exam1
md.pattern(exam1)
summary(missingdata)
summary(mydata)
sd(mydata$gre)
sd(missingdata$gre,na.rm=TRUE)
sd(mydata$gpa)
sd(missingdata$gpa,na.rm=TRUE)
sd(mydata$rank)
sd(missingdata$rank,na.rm=TRUE)

library(imputeTS)
rank_miss_tag<-ifelse(is.na(missingdata$rank),1,0)
gre_miss_tag<-ifelse(is.na(missingdata$gre),1,0)
gpa_miss_tag<-ifelse(is.na(missingdata$gpa),1,0)
imputemeangre<-na_mean(missingdata$gre,option="mean") 
imputemeangpa<-na_mean(missingdata$gpa,option="mean")
imputemeanrank<-na_mean(missingdata$rank,option="mode")
imputemean<-data.frame("admit"=c(mydata$admit),"gre"=c(imputemeangre),"gpa"=c(imputemeangpa),"rank"=c(imputemeanrank))
imputemean
imputemeangpa[tmiss2]
mydata$gpa[tmiss2]
imputemeangre[tmiss1]
mydata$gre[tmiss1]
imputemeanrank[tmiss3]
mydata$rank[tmiss3]

library(DMwR)
missingdata$rank<-as.factor(missingdata$rank)
knnOutput<-knnImputation(missingdata,k=9)
knnOutput[tmiss3,4]
mydata$rank[tmiss3]
table(mydata$rank[tmiss3],knnOutput[tmiss3,4])
mean(knnOutput[tmiss3,4]==mydata$rank[tmiss3])
lm10<-glm(admit~gpa+gre+rank,data=knnOutput,family="binomial")
summary(lm10)
summary(knnOutput)
mydata$rank<-as.factor(mydata$rank)
summary(mydata)
sd(knnOutput$gre)
library(mice)
imp<-mice(missingdata,m=5)
require(colorspace)
require(grid)
require(data.table)
library(VIM)
miss_plot<-aggr(missingdata,col=c("purple","yellow"),numbers=TRUE,sortVars=TRUE,
                labels=names(missingdata),cex.axis=0.7,
                gap=3,ylab=c("Missing data","Pattern"))
par(mfrow=c(1,2))
marginplot(missingdata[c(2,4)],xlab="gre")
imputed_data<-mice(missingdata,m=5,maxit = 100,meth='pmm',seed=500)
summary(imputed_data)
imputed_data$imp$rank
xyplot(imputed_data,gre~rank,pch=18,cex=1)
densityplot(imputed_data)
stripplot(imputed_data,pch=20,cex=1.2)
completeData<-complete(imputed_data)
completeData[tmiss1,2]
mydata[tmiss1,2]

fit<-with(imputed_data,lm(admit~rank+gre+gpa),family="binomial");pool(fit)
summary(pool(fit))
summary(lm1)

