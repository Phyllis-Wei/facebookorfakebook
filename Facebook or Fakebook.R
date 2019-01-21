# STAT318 Final Project R Script 
# Donna Gan and Phyllis Wei
# December 2018

d<-read.csv(file="facebookac.csv", header=TRUE, na.strings = c(" ","NA"))
dim(d)

# Step 1: Data Cleaning
# identify missing values
sum(is.na(d))
colSums(is.na(d))
which(is.na(d), arr.ind=TRUE)
# dealing with missing values:
# missing value in d$relationship
d$relationship<-as.factor(d$relationship)
levels(d$relationship)<-c(levels(d$relationship),"missing")
d$relationship[is.na(d$relationship)]<-"missing"
summary(d$relationship)
# drop duplicate observations
d<-d[1:500,]
dim(d)
# relevel response variable: 
y_binary = ifelse(d$Status == "fake",1,0) #fake=1
d$y<-y_binary
table(d$y)# half of the data are real, half are fake
# data exploration
d_real=d[d$Status=='real',]
d_fake=d[d$Status=='fake',]
boxplot(d_real$phototag.,d_fake$phototag., names=c("real","fake"),xlab="account type",ylab="number of photos tagged in",main="number of photos tagged in for real and fake accounts",ylim=c(0,80))
par(mfrow=c(1,1))
boxplot(d_real$group,d_fake$group, names=c("real","fake"),xlab="account type",ylab="number of groups",main="number of groups this person is in for real and fake accounts",ylim=c(0,70))
#explore the association between predictor "group" and the response
m<-glm(formula = y ~ group, data = d[, -1], family = binomial(link = "logit"))
summary(m)

# Step 2: Check Multicollinearity
#install.packages("usdm")
library(usdm)
x = model.matrix(Status~.,data=d)[,-1]
dim(x)
vifstep(x)
# 1 variables from the 26 input variables have collinearity problem: sport 
# drop predictor sport from dataframe
d<-d[,-12]
dim(d)

# Step 3: Variable Selection
# Automatic selection (stepwise AIC and BIC)
#install.packages("MASS")
library(MASS)
# Based on AIC criterion
step(glm(y~., data = d[,-1]),direction = "both",k=2) # Final step AIC=302.1
model1<-glm(formula = y ~ No.Friend + education + about.me + family + gender + relationship + phototag. + checkin + music + film + series + like + group + note + post.shared.post.posted.rate, data = d[, -1], family = binomial(link = "logit")) #p=20
summary(model1) 
M1<-83.371
# Based on BIC criterion
step(glm(y~., data = d[,-1]),direction = "both",k=log(nrow(d))) # Final step BIC=374.14
model2<-glm(formula = y ~ education + about.me + family + gender + relationship + phototag. + checkin + music + like + group + post.shared.post.posted.rate, data = d[, -1], family = binomial(link = "logit")) #p=16
summary(model2) 
M2<-98.454

# Step 4: Model Comparison 
# LRT for goodness of fit
df<-nrow(d)-ncol(d)+1 #df=500-22=478
qchisq(0.95,df) 
1-pchisq(M1,df) #p-value=1
1-pchisq(M2,df) #p-value=1
# lie towards the lower tail, both models are considered similar to the saturated model
# LRT for nested models
qchisq(0.95,4) #df=15-11=4
1-pchisq(M2-M1,4) #p-value=0.004532129
# AIC model is better than BIC model

# Performance Metrics on Model1 and Model2: AUC, Sensitivity, Specificity, Accuracy, Precision, F-measure
#install.packages("pROC")
#install.packages("ROCR")
library(pROC)
library(ROCR)
predict1 = predict(model1, type="response")
tapply(predict1, d$y, mean)
#Confusion Matrix
CF_Matrix1<-table(d$y, predict1 > 0.5)
#sensitivity1<-245/(5+245)
sensitivity1<-CF_Matrix1[2,2]/(CF_Matrix1[2,1] + CF_Matrix1[2,2])
#specificity1<-236/(236+14)
specificity1<-CF_Matrix1[1,1]/(CF_Matrix1[1,1] + CF_Matrix1[1,2])
#accuracy1<-(245+236)/(236+14+5+245)
accuracy1<-(CF_Matrix1[1,1]+CF_Matrix1[2,2])/(CF_Matrix1[1,1] + CF_Matrix1[1,2] + CF_Matrix1[2,1] + CF_Matrix1[2,2])
#precision1<-245/(245+14)
precision1<-CF_Matrix1[2,2]/(CF_Matrix1[2,2] + CF_Matrix1[1,2])
#F-measure
Fmeasure1<-2*(sensitivity1*precision1)/(sensitivity1+precision1)
#AUC
AUC1<-auc(d$y,predict1)
#ROC curve
ROCRpred1 = prediction(predict1, d$y)
ROCRperf1 = performance(ROCRpred1, "tpr", "fpr")
plot(ROCRperf1, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))
abline(a=0, b=1)

predict2 = predict(model2, type="response")
tapply(predict2, d$y, mean)
#Confusion Matrix
CF_Matrix2<-table(d$y, predict2 > 0.5)
#sensitivity2<-243/(7+243)
sensitivity2<-CF_Matrix2[2,2]/(CF_Matrix2[2,1] + CF_Matrix2[2,2])
#specificity2<-236/(236+14)
specificity2<-CF_Matrix2[1,1]/(CF_Matrix2[1,1] + CF_Matrix2[1,2])
#accuracy2<-(243+236)/(236+14+7+243)
accuracy2<-(CF_Matrix2[1,1]+CF_Matrix2[2,2])/(CF_Matrix2[1,1] + CF_Matrix2[1,2] + CF_Matrix2[2,1] + CF_Matrix2[2,2])
#precision2<-243/(243+14)
precision2<-CF_Matrix2[2,2]/(CF_Matrix2[2,2] + CF_Matrix2[1,2])
#F-measure
Fmeasure2<-2*(sensitivity2*precision2)/(sensitivity2+precision2)
#AUC
AUC2<-auc(d$y,predict2)
#ROC curve
ROCRpred2 = prediction(predict2, d$y)
ROCRperf2 = performance(ROCRpred2, "tpr", "fpr")
plot(ROCRperf2, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))
abline(a=0, b= 1)

# Performance Metrics on Model1 and Model2 with 10-fold cross validation
n = dim(d)[1]
subset1 = sample(1:n,50,replace=FALSE)
subset2 = sample(setdiff(1:n,subset1), 50, replace=FALSE)
subset3 = sample(setdiff(1:n,c(subset1,subset2)),50,replace=FALSE)
subset4 = sample(setdiff(1:n,c(subset1,subset2,subset3)),50,replace=FALSE)
subset5 = sample(setdiff(1:n,c(subset1,subset2,subset3,subset4)),50,replace=FALSE)
subset6 = sample(setdiff(1:n,c(subset1,subset2,subset3,subset4,subset5)),50,replace=FALSE)
subset7 = sample(setdiff(1:n,c(subset1,subset2,subset3,subset4,subset5,subset6)),50,replace=FALSE)
subset8 = sample(setdiff(1:n,c(subset1,subset2,subset3,subset4,subset5,subset6,subset7)),50,replace=FALSE)
subset9 = sample(setdiff(1:n,c(subset1,subset2,subset3,subset4,subset5,subset6,subset7,subset8)),50,replace=FALSE)
subset10 = setdiff(1:n,c(subset1,subset2,subset3,subset4,subset5,subset6,subset7,subset8,subset9))

index = list(subset1,subset2,subset3,subset4,subset5,subset6,subset7,subset8,subset9,subset10)
cvAUC_score1 = 0
cvAUC_score2 = 0
cvSensitivity1 = 0
cvSensitivity2 = 0
cvSpecificity1 = 0
cvSpecificity2 = 0
cvAccuracy1 = 0
cvAccuracy2 = 0
cvPrecision1 = 0
cvPrecision2 = 0
for(i in 1:10)
{
  predictTest1 = predict(model1, type = "response", newdata = d[index[[i]],])
  #cvAUC
  cvAUC_score1  = cvAUC_score1+(1/10)*auc(d[index[[i]],]$y,predictTest1)
  print(auc(d[index[[i]],]$y,predictTest1))
  CF_Matrix_cv1<-table(d[index[[i]],]$y, predictTest1 > 0.5)
  #cvSensitivity
  cvSensitivity1 = cvSensitivity1+(1/10)*CF_Matrix_cv1[2,2]/(CF_Matrix_cv1[2,1] + CF_Matrix_cv1[2,2])
  print(CF_Matrix_cv1[2,2]/(CF_Matrix_cv1[2,1] + CF_Matrix_cv1[2,2]))
  #cvSpecificity
  cvSpecificity1 = cvSpecificity1+(1/10)*CF_Matrix_cv1[1,1]/(CF_Matrix_cv1[1,1] + CF_Matrix_cv1[1,2])
  print(CF_Matrix_cv1[1,1]/(CF_Matrix_cv1[1,1] + CF_Matrix_cv1[1,2]))
  #cvAccuracy
  cvAccuracy1 = cvAccuracy1+(1/10)*(CF_Matrix_cv1[1,1]+CF_Matrix_cv1[2,2])/(CF_Matrix_cv1[1,1] + CF_Matrix_cv1[1,2] + CF_Matrix_cv1[2,1] + CF_Matrix_cv1[2,2])
  print((CF_Matrix_cv1[1,1]+CF_Matrix_cv1[2,2])/(CF_Matrix_cv1[1,1] + CF_Matrix_cv1[1,2] + CF_Matrix_cv1[2,1] + CF_Matrix_cv1[2,2]))
  #cvPrecision
  cvPrecision1 = cvPrecision1+(1/10)*CF_Matrix_cv1[2,2]/(CF_Matrix_cv1[2,2] + CF_Matrix_cv1[1,2])
  print(CF_Matrix_cv1[2,2]/(CF_Matrix_cv1[2,2] + CF_Matrix_cv1[1,2]))
  
  
  predictTest2 = predict(model2, type = "response", newdata = d[index[[i]],])
  #cvAUC
  cvAUC_score2  = cvAUC_score2+(1/10)*auc(d[index[[i]],]$y,predictTest2)
  print(auc(d[index[[i]],]$y,predictTest2))
  CF_Matrix_cv2<-table(d[index[[i]],]$y, predictTest2 > 0.5)
  #cvSensitivity
  cvSensitivity2 = cvSensitivity2+(1/10)*CF_Matrix_cv2[2,2]/(CF_Matrix_cv2[2,1] + CF_Matrix_cv2[2,2])
  print(CF_Matrix_cv2[2,2]/(CF_Matrix_cv2[2,1] + CF_Matrix_cv2[2,2]))
  #cvSpecificity
  cvSpecificity2 = cvSpecificity2+(1/10)*CF_Matrix_cv2[1,1]/(CF_Matrix_cv2[1,1] + CF_Matrix_cv2[1,2])
  print(CF_Matrix_cv2[1,1]/(CF_Matrix_cv2[1,1] + CF_Matrix_cv2[1,2]))
  #cvAccuracy
  cvAccuracy2 = cvAccuracy2+(1/10)*(CF_Matrix_cv2[1,1]+CF_Matrix_cv2[2,2])/(CF_Matrix_cv2[1,1] + CF_Matrix_cv2[1,2] + CF_Matrix_cv2[2,1] + CF_Matrix_cv2[2,2])
  print((CF_Matrix_cv2[1,1]+CF_Matrix_cv2[2,2])/(CF_Matrix_cv2[1,1] + CF_Matrix_cv2[1,2] + CF_Matrix_cv2[2,1] + CF_Matrix_cv2[2,2]))
  #cvPrecision
  cvPrecision2 = cvPrecision2+(1/10)*CF_Matrix_cv2[2,2]/(CF_Matrix_cv2[2,2] + CF_Matrix_cv2[1,2])
  print(CF_Matrix_cv2[2,2]/(CF_Matrix_cv2[2,2] + CF_Matrix_cv2[1,2]))
}

cvFmeasure1 <- 2*(cvSensitivity1*cvPrecision1)/(cvSensitivity1+cvPrecision1)
cvFmeasure2 <- 2*(cvSensitivity2*cvPrecision2)/(cvSensitivity2+cvPrecision2)

cvAUC_score1
cvAUC_score2
cvSensitivity1
cvSensitivity2
cvSpecificity1
cvSpecificity2
cvAccuracy1
cvAccuracy2
cvPrecision1
cvPrecision2
cvFmeasure1
cvFmeasure2

# Step 5: Model diagostics
par(mfrow=c(2,2))
plot(model1)
# delta deviance plot
par(mfrow=c(1,1))
diagplot.logistic = function (model, lms = summary(model), lmi = lm.influence(model))
{
  h <- lmi$hat 
  dr <- residuals(model, type = "deviance") 
  dD <- dr^2/(1 - h) 
  fv <- model$fitted.values 
  plot(dD, main = "Plot of deltaDev vs. Index Number", xlab = "Index Number")
  points(index[dD > 6], dD[dD > 6], col = "blue")
  identify(index, dD, cex = 0.75)
  par(mfrow = c(1, 1))
  invisible()
}
diagplot.logistic(model1)
# visualize outlier
d[486,]
library("car")
Boxplot(d$film~d$Status, id=TRUE)
# eliminate outlier
d_new=d[-486,]
dim(d_new)
# generate final_model
final_model=glm(y ~ No.Friend + education + about.me + family + gender + relationship + phototag. + checkin + music + film + series + like + group + note + post.shared.post.posted.rate, data = d_new[, -1], family = binomial(link = "logit"))
summary(final_model)
# performance metric for final_model
predict_final = predict(final_model, type="response")
#Confusion Matrix
CF_Matrix_final<-table(d_new$y, predict_final > 0.5)
sensitivity_final<-CF_Matrix_final[2,2]/(CF_Matrix_final[2,1] + CF_Matrix_final[2,2])
specificity_final<-CF_Matrix_final[1,1]/(CF_Matrix_final[1,1] + CF_Matrix_final[1,2])
accuracy_final<-(CF_Matrix_final[1,1]+CF_Matrix_final[2,2])/(CF_Matrix_final[1,1] + CF_Matrix_final[1,2] + CF_Matrix_final[2,1] + CF_Matrix_final[2,2])
precision_final<-CF_Matrix_final[2,2]/(CF_Matrix_final[2,2] + CF_Matrix_final[1,2])
Fmeasure_final<-2*(sensitivity_final*precision_final)/(sensitivity_final+precision_final)
#AUC
AUC_final<-auc(d_new$y,predict_final)
#ROC curve
ROCRpred_final = prediction(predict_final, d_new$y)
ROCRperf_final = performance(ROCRpred_final, "tpr", "fpr")
plot(ROCRperf_final, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))
abline(a=0, b=1)

# Appendix
# principal component analysis on Model 1
#install.packages("pls")
library("pls")
pcr.fit<-pcr(y ~ No.Friend + education + about.me + family + gender + relationship + phototag. + checkin + music + film + series + like + group + note + post.shared.post.posted.rate, data = d_new[, -1], scale=TRUE, validation="CV")
x_new = model.matrix(y~.,data=d_new[,-c(1,9,10,12,13,17,18)])[,-1]
dim(x_new)
pcr_fit<-prcomp(x_new, scale=TRUE, validation="CV")
summary(pcr.fit)
summary(pcr_fit)
