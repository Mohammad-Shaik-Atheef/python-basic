rm(list=ls(all=T))
library(readr)
emp_data <- read_csv("C:/Users/Mohd Shaik Atheef/Desktop/lms/simple linear regression/emp_data.csv")
View(emp_data)
attach(emp_data)

#EDA process


hist(Salary_hike)
hist(Churn_out_rate)
library(moments)
skewness(Salary_hike)
skewness(Churn_out_rate)
qqnorm(Churn_out_rate)
qqline(Churn_out_rate)
qqnorm(Salary_hike)
qqline(Salary_hike)
#the data is normal
sum(is.na(emp_data))
#no na values
boxplot(Salary_hike)
boxplot(Churn_out_rate)
#no outliers


plot(Salary_hike,Churn_out_rate)
cor(Churn_out_rate,Salary_hike)
#strong corelation 
#-ve corelation

#building the model
reg=lm(Churn_out_rate~Salary_hike)
summary(reg)
#R^2 value is 0.8312 nearly good model
mean(reg$residuals)
rmse_reg=sqrt(mean(reg$residuals^2))
rmse_reg
library(ggplot2)
ggplot(data = emp_data,aes(x=Salary_hike,y=Churn_out_rate))+
         geom_point(color="green")+
         geom_line(color="red",data=emp_data,aes(x=Salary_hike,y=reg$fitted.values))


#split the data
library(caTools)
split=sample.split(Churn_out_rate,SplitRatio = 0.70)
train=subset(emp_data,split==T)
test=subset(emp_data,split==F)
model1=lm(Churn_out_rate~Salary_hike,data = train)
summary(model1)
rmse_reg_train=sqrt(mean(model1$residuals^2))
pred_reg_test=predict(model1,newdata = test)
err_reg_test=test$Churn_out_rate-pred_reg_test
rmse_reg_test=sqrt(mean(err_reg_test^2))
print(c(rmse_reg,rmse_reg_train,rmse_reg_test))


################logirthm model##################33
reg_log=lm(Churn_out_rate~log(Salary_hike))
summary(reg_log)
#R^2 value is 0.8486 nearly good model
mean(reg_log$residuals)
rmse_reg_log=sqrt(mean(reg_log$residuals^2))
rmse_reg_log
#model of splited data
model_log=lm(Churn_out_rate~log(Salary_hike),data = train)
summary(model_log)
rmse_log_train=sqrt(mean(model_log$residuals^2))
pred_log_test=predict(model_log,newdata = test)
err_log_test=test$Churn_out_rate-pred_log_test
rmse_log_test=sqrt(mean(err_log_test^2))
print(c(rmse_reg_log,rmse_log_train,rmse_log_test))


###############exponential data############
reg_exp=lm(log(Churn_out_rate)~Salary_hike)
summary(reg_exp)
#R^2 value is 0.8735 nearly good model
mean(reg_exp$residuals)
err_exp=Churn_out_rate-exp(reg_exp$fitted.values)
rmse_reg_exp=sqrt(mean(err_exp^2))
rmse_reg_exp
#model of splited data
model_exp=lm(log(Churn_out_rate)~Salary_hike,data = train)
summary(model_exp)
err_exp_train=train$Churn_out_rate-exp(model_exp$fitted.values)
rmse_exp_train=sqrt(mean(err_exp_train^2))
pred_exp_test=predict(model_exp,newdata = test)
err_exp_test=test$Churn_out_rate-exp(pred_exp_test)
rmse_exp_test=sqrt(mean(err_exp_test^2))
print(c(rmse_reg_exp,rmse_exp_train,rmse_exp_test))
#among 3 transformation exponential  model gives good fit with high strength i.e 0.8735
#which is good strength