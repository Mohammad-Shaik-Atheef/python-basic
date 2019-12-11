rm(list=ls(all=T))
library(readr)
Salary_Data <- read_csv("C:/Users/Mohd Shaik Atheef/Desktop/lms/simple linear regression/Salary_Data.csv")
View(Salary_Data)
attach(Salary_Data)

#EDA process


hist(Salary)
hist(YearsExperience)
library(moments)
skewness(Salary)
skewness(YearsExperience)
qqnorm(YearsExperience)
qqline(YearsExperience)
qqnorm(Salary)
qqline(Salary)
#the data is normal
sum(is.na(Salary_Data))
#no na values
boxplot(Salary)
boxplot(YearsExperience)
#no outliers


plot(Salary,YearsExperience)
cor(YearsExperience,Salary)
#strong corelation 
#+ve corelation

#building the model
reg=lm(Salary~YearsExperience)
summary(reg)
#R^2 value is 0.957  good model
mean(reg$residuals)
rmse_reg=sqrt(mean(reg$residuals^2))
rmse_reg
library(ggplot2)
ggplot(data = Salary_Data,aes(x=YearsExperience,y=Salary))+
  geom_point(color="green")+
  geom_line(color="red",data=Salary_Data,aes(x=YearsExperience,y=reg$fitted.values))


#split the data
library(caTools)
split=sample.split(Salary,SplitRatio = 0.70)
train=subset(Salary_Data,split==T)
test=subset(Salary_Data,split==F)
model1=lm(Salary~YearsExperience,data = train)
summary(model1)
rmse_reg_train=sqrt(mean(model1$residuals^2))
pred_reg_test=predict(model1,newdata = test)
err_reg_test=test$Salary-pred_reg_test
rmse_reg_test=sqrt(mean(err_reg_test^2))
print(c(rmse_reg,rmse_reg_train,rmse_reg_test))


################logirthm model##################33
reg_log=lm(Salary~log(YearsExperience))
summary(reg_log)
#R^2 value is 0.8539  good model
mean(reg_log$residuals)
rmse_reg_log=sqrt(mean(reg_log$residuals^2))
rmse_reg_log
#model of splited data
model_log=lm(Salary~log(YearsExperience),data = train)
summary(model_log)
rmse_log_train=sqrt(mean(model_log$residuals^2))
pred_log_test=predict(model_log,newdata = test)
err_log_test=test$Salary-pred_log_test
rmse_log_test=sqrt(mean(err_log_test^2))
print(c(rmse_reg_log,rmse_log_train,rmse_log_test))


###############exponential data############
reg_exp=lm(log(Salary)~YearsExperience)
summary(reg_exp)
#R^2 value is 0.932  good model
mean(reg_exp$residuals)
err_exp=Salary-exp(reg_exp$fitted.values)
rmse_reg_exp=sqrt(mean(err_exp^2))
rmse_reg_exp
#model of splited data
model_exp=lm(log(Salary)~YearsExperience,data = train)
summary(model_exp)
err_exp_train=train$Salary-exp(model_exp$fitted.values)
rmse_exp_train=sqrt(mean(err_exp_train^2))
pred_exp_test=predict(model_exp,newdata = test)
err_exp_test=test$Salary-exp(pred_exp_test)
rmse_exp_test=sqrt(mean(err_exp_test^2))
print(c(rmse_reg_exp,rmse_exp_train,rmse_exp_test))


##############polynomial 2 degree################
reg_poly=lm(Salary~YearsExperience+I(YearsExperience^2))
summary(reg_poly)
#strength is 0.957 good strength
rmse_poly=sqrt(mean(reg_poly$residuals^2))
rmse_poly
#spliting of model
model_poly=lm(Salary~YearsExperience+I(YearsExperience^2),data = train)
summary(model_poly)
rmse_poly_train=sqrt(mean(model_poly$residuals^2))
rmse_poly_train
pred_poly_test=predict(model_poly,newdata = test)
err_poly_test=test$Salary-pred_poly_test
rmse_poly_test=sqrt(mean(err_poly_test^2))
print(c(rmse_poly,rmse_poly_train,rmse_poly_test))


##############polynomial 3 degree################
reg_poly2=lm(Salary~YearsExperience+I(YearsExperience^2)+I(YearsExperience^3))
summary(reg_poly2)
#strength is 0.963 good strength
rmse_poly2=sqrt(mean(reg_poly2$residuals^2))
rmse_poly2
#spliting of model
model_poly2=lm(Salary~YearsExperience+I(YearsExperience^2)+I(YearsExperience^3),data = train)
summary(model_poly2)
rmse_poly2_train=sqrt(mean(model_poly2$residuals^2))
rmse_poly2_train
pred_poly2_test=predict(model_poly2,newdata = test)
err_poly2_test=test$Salary-pred_poly2_test
rmse_poly2_test=sqrt(mean(err_poly2_test^2))
print(c(rmse_poly2,rmse_poly2_train,rmse_poly2_test))
#third degree polynomial is providing nearly best fit line 
#with strength of 96%