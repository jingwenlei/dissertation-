community1<-read.csv("C:/Users/Jingwen.lei/Desktop/community.csv",header=TRUE,fill=TRUE) #This file is downloaded from http://www-stat.wharton.upenn.edu/~dsmall/stat921-f09/
head(community1)
#data after zero value replacement
community<-data.frame( "twoyr"=c(community1$twoyr),"gender"=c(community1$female),"black"=c(community1$black),"hispanic"=c(community1$hispanic),      
                              "bytest"=c(community1$bytest), "dadmiss"=c(community1$dadmiss),"dadvoc"=c(community1$dadvoc),"dadsome"=c(community1$dadsome),"dadcoll"=c(community1$dadcoll), 
                              "mommiss"=c(community1$mommiss),"momvoc"=c(community1$momvoc),"momsome"=c(community1$momsome),"momcoll"=c(community1$momcoll),"fincome"=c(community1$fincome),       
                              "fincmiss"=c(community1$fincmiss),"ownhome"=c(community1$ownhome),"perwhite"=c(community1$perwhite), "urban"=c(community1$urban),"cue80"=c(community1$cue80),        
                              "stypc80"=c(community1$stypc80), "stwmfg80"=c(community1$stwmfg80),"tuition2"=c(community1$tuition2),"tuition4"=c(community1$tuition4),"dist2yr"=c(community1$dist2yr),       
                              "dist4yr"=c(community1$dist4yr),"region_midwest"=c(community1$region_midwest), "region_south"=c(community1$region_south),"region_west"=c(community1$region_west) )


community$stypc80<-as.numeric(community$stypc80)
community$bytest<-as.numeric(community$bytest)
community$fincome<-as.numeric(community$fincome)
community$perwhite<-as.numeric(community$perwhite)
community$cue80<-as.numeric(community$cue80)
community$stwmfg80<-as.numeric(community$stwmfg80)
community$tuition2<-as.numeric(community$tuition2)
community$tuition4<-as.numeric(community$tuition4)
community$dist2yr<-as.numeric(community$dist2yr)
community$dist4yr<-as.numeric(community$dist4yr)
community$twoyr<-as.factor(community$twoyr)
community$gender<-as.factor(community$gender)
community$black<-as.factor(community$black)
community$hispanic<-as.factor(community$hispanic)
community$dadmiss<-as.factor(community$dadmiss)
community$dadvoc<-as.factor(community$dadvoc)
community$dadsome<-as.factor(community$dadsome)
community$dadcoll<-as.factor(community$dadcoll)
community$mommiss<-as.factor(community$mommiss)
community$momvoc<-as.factor(community$momvoc)
community$momsome<-as.factor(community$momsome)
community$momcoll<-as.factor(community$momcoll)
community$fincmiss<-as.factor(community$fincmiss)
community$ownhome<-as.factor(community$ownhome)
community$urban<-as.factor(community$urban)
community$region_midwest<-as.factor(community$region_midwest)
community$region_south<-as.factor(community$region_south)
community$region_west<-as.factor(community$region_west)
summary(community)

set.seed(1231)
fit1<-glm(twoyr~gender+black+hispanic+bytest+dadmiss+dadvoc+dadsome+dadcoll+mommiss+momvoc+momsome+momcoll
          +fincome+fincmiss+ownhome+perwhite+urban+cue80+stypc80+stwmfg80+tuition2+tuition4+dist2yr+dist4yr+region_midwest+region_south+region_west,
          data=community,family=binomial(link=logit))
summary(fit1)
#training error
prob<-predict(fit1,data=community,type="response")
pred<-ifelse(prob > 0.5, "1","0")
table(pred,community$twoyr)
mean(pred==community$twoyr)
#0.7270678

#stepwise    16 predictors
step(fit1,direction="backward")
fit2<-glm(formula = twoyr ~ gender + black + bytest + dadcoll + mommiss + 
                          momcoll + fincome + perwhite + stypc80 + stwmfg80 + tuition4 + 
                          dist2yr + dist4yr + region_midwest + region_south + region_west, 
                        family = binomial(link = logit), data = community)
summary(fit2)
prob<-predict(fit2,data=community,type="response")
pred<-ifelse(prob > 0.5, "1","0")
table(pred,community$twoyr)
mean(pred==community$twoyr)
#0.7263917

#roc curve
par(mfrow=c(1,2))
library(ROCR)
pred<-prediction(fitted(fit1),community$twoyr)
perf<-performance(pred,"tpr","fpr")            
plot(perf)
performance(pred,"auc")
abline(a=0,b=1)
#AUC=0.7791599
pred<-prediction(fitted(fit2),community$twoyr)
perf<-performance(pred,"tpr","fpr")            
plot(perf)
performance(pred,"auc")
abline(a=0,b=1)
#AUC=0.778768



#change zero value replacement to data set with missing value
missingdad<-rep(0,4437)
missingdad<-ifelse(community$dadmiss ==1,NA,missingdad)
missingdad<-ifelse(community$dadvoc ==1,1,missingdad)
missingdad<-ifelse(community$dadsome ==1,2,missingdad)
missingdad<-ifelse(community$dadcoll ==1,3,missingdad)

missingmom<-rep(0,4437)
missingmom<-ifelse(community$mommiss ==1,NA,missingmom)
missingmom<-ifelse(community$momvoc ==1,1,missingmom)
missingmom<-ifelse(community$momsome ==1,2,missingmom)
missingmom<-ifelse(community$momcoll ==1,3,missingmom)

missingfincome<-ifelse(community$fincmiss ==1,NA,community$fincome)
missingcommunity<-data.frame( "twoyr"=c(community1$twoyr),"gender"=c(community1$female),"black"=c(community1$black),"hispanic"=c(community1$hispanic),      
                              "bytest"=c(community1$bytest), "dad"=c(missingdad), 
                              "mom"=c(missingmom),          "fincome"=c(missingfincome),       
                             "ownhome"=c(community1$ownhome),"perwhite"=c(community1$perwhite), "urban"=c(community1$urban),"cue80"=c(community1$cue80),        
                              "stypc80"=c(community1$stypc80), "stwmfg80"=c(community1$stwmfg80),"tuition2"=c(community1$tuition2),"tuition4"=c(community1$tuition4),"dist2yr"=c(community1$dist2yr),       
                              "dist4yr"=c(community1$dist4yr),"region_midwest"=c(community1$region_midwest), "region_south"=c(community1$region_south),"region_west"=c(community1$region_west) )
summary(missingcommunity)
missingcommunity$mom<-as.factor(missingcommunity$mom)
missingcommunity$dad<-as.factor(missingcommunity$dad)
missingcommunity$twoyr<-as.factor(missingcommunity$twoyr)
missingcommunity$stypc80<-as.numeric(missingcommunity$stypc80)
missingcommunity$bytest<-as.numeric(missingcommunity$bytest)
missingcommunity$fincome<-as.numeric(missingcommunity$fincome)
missingcommunity$perwhite<-as.numeric(missingcommunity$perwhite)
missingcommunity$cue80<-as.numeric(missingcommunity$cue80)
missingcommunity$stwmfg80<-as.numeric(missingcommunity$stwmfg80)
missingcommunity$tuition2<-as.numeric(missingcommunity$tuition2)
missingcommunity$tuition4<-as.numeric(missingcommunity$tuition4)
missingcommunity$dist2yr<-as.numeric(missingcommunity$dist2yr)
missingcommunity$dist4yr<-as.numeric(missingcommunity$dist4yr)
missingcommunity$gender<-as.factor(missingcommunity$gender)
missingcommunity$black<-as.factor(missingcommunity$black)
missingcommunity$hispanic<-as.factor(missingcommunity$hispanic)
missingcommunity$ownhome<-as.factor(missingcommunity$ownhome)
missingcommunity$urban<-as.factor(missingcommunity$urban)
missingcommunity$region_midwest<-as.factor(missingcommunity$region_midwest)
missingcommunity$region_south<-as.factor(missingcommunity$region_south)
missingcommunity$region_west<-as.factor(missingcommunity$region_west)
summary(missingcommunity)
library(mice)
require(colorspace)
require(grid)
require(data.table)
library(VIM)
md.pattern(missingcommunity[,c(6,7,8)])
miss_plot<-aggr(missingcommunity,col=c("purple","yellow"),numbers=TRUE,sortVars=TRUE,
                labels=names(missingcommunity),cex.axis=0.7,
                gap=3,ylab=c("Missing data","Pattern"))
par(mfrow=c(1,3))
marginplot(missingcommunity[c(6,7)],xlab="dad")
marginplot(missingcommunity[c(6,8)],xlab="dad")
marginplot(missingcommunity[c(7,8)],xlab="mom")
summary(missingcommunity[,c(6,7,8)])


#complete case analysis 
fit3<-glm(formula = twoyr ~ gender + black + hispanic + bytest + dad + 
          mom + fincome + ownhome + perwhite + urban + cue80 + stypc80 + 
          stwmfg80 + tuition2 + tuition4 + dist2yr + dist4yr + region_midwest + 
          region_south + region_west, family = binomial(link = logit), 
        data = missingcommunity)
summary(fit3)
completecom<-na.omit(missingcommunity)
prob<-predict(fit3,newdata=completecom,type="response")
pred<-ifelse(prob > 0.5, "1","0")
table(pred,completecom$twoyr)
mean(pred==completecom$twoyr)    #0.7283272
#stepwise     16 predictors
step(fit3,direction="backward")
fit4<-glm(formula = twoyr ~ gender + black + bytest + dad + mom + fincome + 
            perwhite + cue80 + stypc80 + stwmfg80 + tuition4 + dist2yr + 
            dist4yr + region_midwest + region_south + region_west, 
          family = binomial(link = logit), data = missingcommunity)
summary(fit4)
prob<-predict(fit4,newdata=completecom,type="response")
pred<-ifelse(prob > 0.5, "1","0")
table(pred,completecom$twoyr)
mean(pred==completecom$twoyr)   #0.729243
#roc curve
pred<-prediction(fitted(fit3),completecom$twoyr)
perf<-performance(pred,"tpr","fpr")            
plot(perf)
performance(pred,"auc")
abline(a=0,b=1)
#AUC=0.7751195
pred<-prediction(fitted(fit4),completecom$twoyr)
perf<-performance(pred,"tpr","fpr")            
plot(perf)
performance(pred,"auc")
abline(a=0,b=1)
#AUC=0.7751826


#mean imputation
library(imputeTS)
id<-seq(1,4437,1)
caseid<-cbind(id,missingcommunity)
mom_miss_tag<-ifelse(is.na(missingcommunity$mom),caseid$id,NA)
mom_miss_tag<-na.omit(mom_miss_tag)
mom_miss_tag
dad_miss_tag<-ifelse(is.na(missingcommunity$dad),caseid$id,NA)
dad_miss_tag<-na.omit(dad_miss_tag)
fin_miss_tag<-ifelse(is.na(missingcommunity$fincome),caseid$id,NA)
fin_miss_tag<-na.omit(fin_miss_tag)

num_mom<-as.numeric(missingcommunity$mom)
num_dad<-as.numeric(missingcommunity$dad)
imputemodemom<-na_mean(num_mom,option="mode")-1 
imputemodedad<-na_mean(num_dad,option="mode")-1
imputemeanfincome<-na_mean(missingcommunity$fincome,option="mean")
imputemean<-data.frame("twoyr"=c(community1$twoyr),"gender"=c(community1$female),"black"=c(community1$black),"hispanic"=c(community1$hispanic),      
                       "bytest"=c(community1$bytest), "dad"=c(imputemodedad), 
                       "mom"=c(imputemodemom),          "fincome"=c(imputemeanfincome),       
                       "ownhome"=c(community1$ownhome),"perwhite"=c(community1$perwhite), "urban"=c(community1$urban),"cue80"=c(community1$cue80),        
                       "stypc80"=c(community1$stypc80), "stwmfg80"=c(community1$stwmfg80),"tuition2"=c(community1$tuition2),"tuition4"=c(community1$tuition4),"dist2yr"=c(community1$dist2yr),       
                       "dist4yr"=c(community1$dist4yr),"region_midwest"=c(community1$region_midwest), "region_south"=c(community1$region_south),"region_west"=c(community1$region_west))
imputemean$twoyr<-as.factor(imputemean$twoyr)
imputemean$stypc80<-as.numeric(imputemean$stypc80)
imputemean$bytest<-as.numeric(imputemean$bytest)
imputemean$fincome<-as.numeric(imputemean$fincome)
imputemean$perwhite<-as.numeric(imputemean$perwhite)
imputemean$cue80<-as.numeric(imputemean$cue80)
imputemean$stwmfg80<-as.numeric(imputemean$stwmfg80)
imputemean$tuition2<-as.numeric(imputemean$tuition2)
imputemean$tuition4<-as.numeric(imputemean$tuition4)
imputemean$dist2yr<-as.numeric(imputemean$dist2yr)
imputemean$dist4yr<-as.numeric(imputemean$dist4yr)
imputemean$gender<-as.factor(imputemean$gender)
imputemean$black<-as.factor(imputemean$black)
imputemean$hispanic<-as.factor(imputemean$hispanic)
imputemean$dad<-as.factor(imputemean$dad)
imputemean$mom<-as.factor(imputemean$mom)
imputemean$ownhome<-as.factor(imputemean$ownhome)
imputemean$urban<-as.factor(imputemean$urban)
imputemean$region_midwest<-as.factor(imputemean$region_midwest)
imputemean$region_south<-as.factor(imputemean$region_south)
imputemean$region_west<-as.factor(imputemean$region_west)
summary(imputemean)

fit5<-glm(formula = twoyr ~ gender + black + hispanic + bytest + dad + 
            mom + fincome + ownhome + perwhite + urban + cue80 + stypc80 + 
            stwmfg80 + tuition2 + tuition4 + dist2yr + dist4yr + region_midwest + 
            region_south + region_west, family = binomial(link = logit), 
          data = imputemean)
summary(fit5)
prob<-predict(fit5,data=imputemean,type="response")
pred<-ifelse(prob > 0.5, "1","0")
table(pred,imputemean$twoyr)
mean(pred==imputemean$twoyr)   #0.7286455
#roc curve
pred<-prediction(fitted(fit5),imputemean$twoyr)
perf<-performance(pred,"tpr","fpr")            
plot(perf)
performance(pred,"auc")
abline(a=0,b=1)
#AUC=0.7794417

fit8<-glm(twoyr~1,data=imputemean,family="binomial"(link=logit))
step(fit8,scope=list(lower=fit8,upper=fit5),direction="both",trace=TRUE)

suggest1<-glm(formula = twoyr ~ bytest + region_west + black + dist2yr + 
                dist4yr + dad + stypc80 + perwhite + region_south + region_midwest + 
                stwmfg80 + mom + fincome + tuition4 + gender, family = binomial(link = logit), 
              data = imputemean)
momcoll<-community1$momcoll
dadcoll<-community1$dadcoll
imputemean<-cbind(imputemean,momcoll,dadcoll)
imputemean$dadcoll<-as.factor(imputemean$dadcoll)
imputemean$momcoll<-as.factor(imputemean$momcoll)
summary(imputemean)
simple<-glm(formula = twoyr ~ gender + black + hispanic + bytest + dadcoll + 
              momcoll + fincome + ownhome + perwhite + urban + cue80 + stypc80 + 
              stwmfg80 + tuition2 + tuition4 + dist2yr + dist4yr + region_midwest + 
              region_south + region_west, family = binomial(link = logit), 
            data = imputemean)

step(fit8,scope=list(lower=fit8,upper=simple),direction="both",trace=TRUE)
suggest2<-glm(formula = twoyr ~ bytest + region_west + black + dist2yr + 
                dadcoll + stypc80 + dist4yr + perwhite + region_south + momcoll + 
                region_midwest + stwmfg80 + fincome + tuition4 + gender, 
              family = binomial(link = logit), data = imputemean)
summary(suggest2)
mod1<-glm(formula = twoyr ~ bytest, family = binomial(link = logit), 
          data = imputemean)
mod2<-glm(formula = twoyr ~ bytest + region_west, family = binomial(link = logit), 
          data = imputemean)
mod3<-glm(formula = twoyr ~ bytest + region_west + black, family = binomial(link = logit), 
          data = imputemean)
mod4<-glm(formula = twoyr ~ bytest + region_west + black + dist2yr, family = binomial(link = logit), 
          data = imputemean)
mod5<-glm(formula = twoyr ~ bytest + region_west + black + dist2yr + 
            dist4yr, family = binomial(link = logit), data = imputemean)
mod6<-glm(formula = twoyr ~ bytest + region_west + black + dist2yr + 
            dist4yr + dad, family = binomial(link = logit), data = imputemean)
mod7<-glm(formula = twoyr ~ bytest + region_west + black + dist2yr + 
            dist4yr + dad + stypc80, family = binomial(link = logit), data = imputemean)
mod8<-glm(formula = twoyr ~ bytest + region_west + black + dist2yr + 
            dist4yr + dad + stypc80 + perwhite, family = binomial(link = logit), data = imputemean)
mod9<-glm(formula = twoyr ~ bytest + region_west + black + dist2yr + 
            dist4yr + dad + stypc80 + perwhite + region_south, family = binomial(link = logit), data = imputemean)
mod10<-glm(formula = twoyr ~ bytest + region_west + black + dist2yr + 
             dist4yr + dad + stypc80 + perwhite + region_south + region_midwest, family = binomial(link = logit), 
           data = imputemean)
mod11<-glm(formula = twoyr ~ bytest + region_west + black + dist2yr + 
             dist4yr + dad + stypc80 + perwhite + region_south + region_midwest + 
             stwmfg80, family = binomial(link = logit), data = imputemean)
mod12<-glm(formula = twoyr ~ bytest + region_west + black + dist2yr + 
             dist4yr + dad + stypc80 + perwhite + region_south + region_midwest + 
             stwmfg80 + mom, family = binomial(link = logit), data = imputemean)
mod13<-glm(formula = twoyr ~ bytest + region_west + black + dist2yr + 
             dist4yr + dad + stypc80 + perwhite + region_south + region_midwest + 
             stwmfg80 + mom + fincome, family = binomial(link = logit), 
           data = imputemean)
mod14<-glm(formula = twoyr ~ bytest + region_west + black + dist2yr + 
           dist4yr + dad + stypc80 + perwhite + region_south + region_midwest + 
           stwmfg80 + mom + fincome + tuition4, family = binomial(link = logit), data = imputemean)
library(rcompanion)
anova(fit8,mod1,mod2,mod3,mod4,mod5,mod6,mod7,mod8,mod9,mod10,mod11,mod12,mod13,mod14,suggest1,fit5,suggest2)
library(lmtest)
lrtest(suggest2,fit5)

imputemean$twoyr<-as.numeric(imputemean$twoyr)
prob<-predict(mod1,type="response")
cor(imputemean$twoyr,prob)
prob<-predict(mod2,type="response")
cor(imputemean$twoyr,prob)
prob<-predict(mod3,type="response")
cor(imputemean$twoyr,prob)
prob<-predict(mod4,type="response")
cor(imputemean$twoyr,prob)
prob<-predict(mod5,type="response")
cor(imputemean$twoyr,prob)
prob<-predict(mod6,type="response")
cor(imputemean$twoyr,prob)
prob<-predict(mod7,type="response")
cor(imputemean$twoyr,prob)
prob<-predict(mod8,type="response")
cor(imputemean$twoyr,prob)
prob<-predict(mod9,type="response")
cor(imputemean$twoyr,prob)
prob<-predict(mod10,type="response")
cor(imputemean$twoyr,prob)
prob<-predict(mod11,type="response")
cor(imputemean$twoyr,prob)
prob<-predict(mod12,type="response")
cor(imputemean$twoyr,prob)
prob<-predict(mod13,type="response")
cor(imputemean$twoyr,prob)
prob<-predict(mod14,type="response")
cor(imputemean$twoyr,prob)
prob<-predict(suggest1,type="response")
cor(imputemean$twoyr,prob)
prob<-predict(fit5,type="response")
cor(imputemean$twoyr,prob)
prob<-predict(suggest2,type="response")
cor(imputemean$twoyr,prob)

pred<-prediction(fitted(suggest2),imputemean$twoyr)
perf<-performance(pred,"tpr","fpr")            
plot(perf)
performance(pred,"auc")
abline(a=0,b=1)

summary(suggest2)
prob<-predict(suggest2,data=imputemean,type="response")
pred<-ifelse(prob > 0.5, "1","0")
table(pred,imputemean$twoyr)
mean(pred==imputemean$twoyr)   #0.72842
pred<-ifelse(prob > 0.619, "1","0")
table(pred,imputemean$twoyr)
mean(pred==imputemean$twoyr)  #0.71467
pred<-ifelse(prob>0.525,"1","0")
table(pred,imputemean$twoyr)
mean(pred==imputemean$twoyr)#0.7295


#mice imputation
library(mice)
imputed_data<-mice(missingcommunity,m=5,maxit = 50,meth='pmm',seed=400)
summary(imputed_data$data)
#xyplot(imputed_data,fincome~mom+dad,pch=18,cex=1)
densityplot(imputed_data)

#completeData<-complete(imputed_data)

micefit1<-with(data=imputed_data,exp=glm(twoyr~gender+black+hispanic+bytest+dad+mom+fincome+ownhome+
                                    perwhite+urban+cue80+stypc80+stwmfg80+tuition2+tuition4+
                                    dist2yr+dist4yr+region_midwest+region_south+region_west,
                                    family=binomial(link=logit)))
summary(pool(micefit1))




#knn imputation
library(DMwR)
knnimpute<-missingcommunity
knnimpute$dad<-as.factor(knnimpute$dad)
knnimpute$mom<-as.factor(knnimpute$mom)
knnimpute$fincome<-as.numeric(knnimpute$fincome)
knnimpute$stypc80<-as.numeric(knnimpute$stypc80)
knnimpute$bytest<-as.numeric(knnimpute$bytest)
knnimpute$perwhite<-as.numeric(knnimpute$perwhite)
knnimpute$cue80<-as.numeric(knnimpute$cue80)
knnimpute$stwmfg80<-as.numeric(knnimpute$stwmfg80)
knnimpute$tuition2<-as.numeric(knnimpute$tuition2)
knnimpute$tuition4<-as.numeric(knnimpute$tuition4)
knnimpute$dist2yr<-as.numeric(knnimpute$dist2yr)
knnimpute$dist4yr<-as.numeric(knnimpute$dist4yr)
knnimpute$gender<-as.factor(knnimpute$gender)
knnimpute$black<-as.factor(knnimpute$black)
knnimpute$hispanic<-as.factor(knnimpute$hispanic)
knnimpute$ownhome<-as.factor(knnimpute$ownhome)
knnimpute$urban<-as.factor(knnimpute$urban)
knnimpute$region_midwest<-as.factor(knnimpute$region_midwest)
knnimpute$region_south<-as.factor(knnimpute$region_south)
knnimpute$region_west<-as.factor(knnimpute$region_west)
knnOutput<-knnImputation(knnimpute[,-1],k=5)
knnOutput[mom_miss_tag,6]
twoyr<-missingcommunity$twoyr
knnOutput<-cbind(twoyr,knnOutput)
knnOutput$twoyr<-as.factor(knnOutput$twoyr)
summary(knnOutput)  
lm1<-glm(twoyr~gender+black+hispanic+bytest+dad+mom+fincome+ownhome+
               perwhite+urban+cue80+stypc80+stwmfg80+tuition2+tuition4+
               dist2yr+dist4yr+region_midwest+region_south+region_west,
             data=knnOutput,family=binomial(link=logit))
summary(lm1)
prob<-predict(lm1,data=knnOutput,type="response")
pred<-ifelse(prob > 0.5, "1","0")
table(pred,knnOutput$twoyr)
mean(pred==knnOutput$twoyr)   #0.7288709
#roc curve
pred<-prediction(fitted(lm1),knnOutput$twoyr)
perf<-performance(pred,"tpr","fpr")            
plot(perf)
performance(pred,"auc")
abline(a=0,b=1)
#AUC=0.7792295







## other classification 
#linear
num_imp<-imputemean
summary(num_imp)
num_imp$gender<-as.numeric(num_imp$gender)-1
num_imp$black<-as.numeric(num_imp$black)-1
num_imp$hispanic<-as.numeric(num_imp$hispanic)-1
num_imp$ownhome<-as.numeric(num_imp$ownhome)-1
num_imp$urban<-as.numeric(num_imp$urban)-1
num_imp$region_midwest<-as.numeric(num_imp$region_midwest)-1
num_imp$region_south<-as.numeric(num_imp$region_south)-1
num_imp$region_west<-as.numeric(num_imp$region_west)-1
num_imp$momcoll<-as.numeric(num_imp$momcoll)-1
num_imp$dadcoll<-as.numeric(num_imp$dadcoll)-1
num_imp$twoyr<-as.numeric(num_imp$twoyr)-1
dadvoc<-community1$dadvoc
dadsome<-community1$dadsome
momsome<-community1$momsome
momvoc<-community$momvoc
num_imp<-cbind(num_imp,dadvoc,dadsome,momsome,momvoc)
num_imp$dadsome<-as.numeric(num_imp$dadsome)-1
num_imp$momsome<-as.numeric(num_imp$momsome)-1
num_imp$dadvoc<-as.numeric(num_imp$dadsome)-1
num_imp$momvoc<-as.numeric(num_imp$dadsome)-1
glm1<-lm(formula = twoyr ~ gender + black + hispanic + bytest + dadcoll +momsome+momvoc+dadvoc+dadsome+
           momcoll + fincome + ownhome + perwhite + urban + cue80 + 
           stypc80 + stwmfg80 + tuition2 + tuition4 + dist2yr + dist4yr + 
           region_midwest + region_south + region_west, data = num_imp)
summary(glm1)
pred<-prediction(fitted(glm1),imputemean$twoyr)
perf<-performance(pred,"tpr","fpr")            
plot(perf)
performance(pred,"auc")
abline(a=0,b=1)
#AUC=0.7784623
prob<-predict(glm1,data=imputemean,type="response")
pred<-ifelse(prob > 0.5, "1","0")
table(pred,imputemean$twoyr)
mean(pred==imputemean$twoyr)#0.7281947
glm2<-lm(twoyr~1,data=num_imp)
step(glm2,scope=list(lower=glm2,upper=glm1),direction="both",trace=TRUE)


glm3<-lm(formula = twoyr ~ bytest + region_west + black + dist2yr + 
     tuition2 + dadcoll + stypc80 + dist4yr + perwhite + region_south + 
     region_midwest + stwmfg80 + momcoll + fincome + tuition4 + 
     gender, data = num_imp)
summary(glm3)
pred<-prediction(fitted(glm3),imputemean$twoyr)
perf<-performance(pred,"tpr","fpr")            
plot(perf)
performance(pred,"auc")
abline(a=0,b=1)
#AUC=0.7781527
prob<-predict(glm3,data=imputemean,type="response")
pred<-ifelse(prob > 0.5, "1","0")
table(pred,imputemean$twoyr)
mean(pred==imputemean$twoyr)#0.7252648
summary(prob)





#knn classification
library(e1071)
require(caret)
num_com<-imputemean[,-c(22,23)]
num_com$gender<-as.numeric(num_com$gender)-1
num_com$black<-as.numeric(num_com$black)-1
num_com$hispanic<-as.numeric(num_com$hispanic)-1
num_com$dad<-as.numeric(num_com$dad)-1
num_com$mom<-as.numeric(num_com$mom)-1
num_com$fincome<-as.numeric(num_com$fincome)
num_com$ownhome<-as.numeric(num_com$ownhome)-1
num_com$urban<-as.numeric(num_com$urban)-1
num_com$region_midwest<-as.numeric(num_com$region_midwest)-1
num_com$region_south<-as.numeric(num_com$region_south)-1
num_com$region_west<-as.numeric(num_com$region_west)-1
head(num_com)
num_com[,-1]<-scale(num_com[,-1])
head(num_com)
num_com$twoyr<-as.factor(num_com$twoyr)
trainControl<-trainControl(method="cv",number =10)
knnfit<-train(twoyr~.,
              method  ="knn",
              tuneGrid=expand.grid(k=1:100),
              trControl=trControl,
              metric="Accuracy",
              data    = num_com)
knnfit
library(class)

set.seed(1202)
knn.pred=knn(num_com[,-1],num_com[,-1],num_com$twoyr,k=33)
table(knn.pred,num_com$twoyr)
mean(knn.pred==num_com$twoyr)   




