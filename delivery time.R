rm(list=ls(all=T))
#predict delivery time using sorting time
#y=delivery time,x=sorting time
library(readr)
delivery_time <- read_csv("C:/Users/Mohd Shaik Atheef/Desktop/lms/simple linear regression/delivery_time.csv")
View(delivery_time)
#time is continuous variable

#EDA 
#normalization test
hist(delivery_time$`Delivery Time`)
hist(delivery_time$`Sorting Time`)
library(moments)
skewness(delivery_time$`Delivery Time`)
skewness(delivery_time$`Sorting Time`)
#the given data is normal
qqnorm(delivery_time$`Delivery Time`)
qqline(delivery_time$`Delivery Time`)
#outlies
boxplot(delivery_time$`Delivery Time`)
boxplot(delivery_time$`Sorting Time`)
#there are no outliers
sum(is.na(delivery_time))

##scatter plot
plot(delivery_time$`Delivery Time`,delivery_time$`Sorting Time`)
cor(delivery_time$`Delivery Time`,delivery_time$`Sorting Time`)
#+ve corelation
#it is moderate model
cor.test(delivery_time$`Delivery Time`,delivery_time$`Sorting Time`)



#built a model
reg_data<-lm(delivery_time$`Delivery Time`~delivery_time$`Sorting Time`)
summary(reg_data)
#strength is 68.23% moderate
#data is significance
mean(reg_data$residuals)
rmse_data=sqrt(mean(reg_data$residuals^2))
rmse_data
library(ggplot2)
ggplot(data=delivery_time,aes(x=delivery_time$`Sorting Time`,y=delivery_time$`Delivery Time`))+
  geom_point(color="red")+
  geom_line(color="green",data=delivery_time,aes(x=delivery_time$`Sorting Time`,y=reg_data$fitted.values))
#split the data
library(caTools)
split=sample.split(delivery_time$`Delivery Time`,SplitRatio = 0.70)
train=subset(delivery_time,split==TRUE)
test=subset(delivery_time,split==FALSE)
model_data<-lm(`Delivery Time`~`Sorting Time`,data = train)
summary(model_data)
mean(model_data$residuals)
rmse_data_train=sqrt(mean(model_data$residuals^2))
rmse_data_train
pred_data_test=predict(model_data,newdata = test)
err_data_test=delivery_time$`Delivery Time`-pred_data_test
rmse_test_data=sqrt(mean(err_data_test^2))
rmse_test_data
print(c(rmse_data,rmse_data_train,rmse_test_data))
############### logirithm data #################################
#transform data using log 
reg_log=lm(delivery_time$`Delivery Time`~log(delivery_time$`Sorting Time`))
summary(reg_log)
#strength is 69.54% moderate
mean(reg_log$residuals)
rmse_log=sqrt(mean(reg_log$residuals^2))
rmse_log
model_log<-lm(`Delivery Time`~log(`Sorting Time`),data = train)
summary(model_log)
mean(model_log$residuals)
rmse_log_train=sqrt(mean(model_log$residuals^2))
rmse_log_train
pred_log_test=predict(model_log,newdata = test)
err_log_test=delivery_time$`Delivery Time`-pred_log_test
rmse_test_log=sqrt(mean(err_log_test^2))
rmse_test_log
print(c(rmse_log,rmse_log_train,rmse_log_data))




#transforming using exp
reg_exp=lm(log(delivery_time$`Delivery Time`)~delivery_time$`Sorting Time`)
summary(reg_exp)
#strength is 71.09% still moderate
mean(reg_exp$residuals)
err_exp=delivery_time$`Delivery Time`-exp(reg_exp$fitted.values)
rmse_exp=sqrt(mean(err_exp^2))
rmse_exp
model_exp=lm(log(`Delivery Time`)~`Sorting Time`,data=train)
summary(model_exp)
mean(model_exp$residuals)
err_train_exp=train$`Delivery Time`-exp(model_exp$fitted.values)
rmse_exp_train=sqrt(mean(err_train_exp^2))
rmse_exp_train
pred_exp_test=predict(model_exp,newdata = test)
err_test_exp=test$`Delivery Time`-exp(pred_exp_test)
rmse_exp_test=sqrt(mean(err_test_exp^2))
print(c(rmse_exp,rmse_exp_train,rmse_exp_test))
# the rmse of exponential data is nearly equal 
#it is a right fit data
#and strength is also 71% moderate and high when compare to other