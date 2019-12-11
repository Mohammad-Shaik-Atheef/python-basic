#Prepare a classification model using Naive Bayes 
rm(list = ls(all=T))
#for salary data 
library(readr)
#for train data
SalaryData_Train <- read_csv("C:/Users/Mohd Shaik Atheef/Desktop/lms/data science assignments/1 assignments/naive bayes/SalaryData_Train.csv")
View(SalaryData_Train)
#for test data
SalaryData_Test <- read_csv("C:/Users/Mohd Shaik Atheef/Desktop/lms/data science assignments/1 assignments/naive bayes/SalaryData_Test.csv")
View(SalaryData_Test)
str(SalaryData_Train)
summary(SalaryData_Train)
class(SalaryData_Train)

SalaryData_Train$Salary=as.factor(SalaryData_Train$Salary)
prop.table(table(SalaryData_Train$Salary))

#build model on train data
library(e1071)
salary_classifer=naiveBayes(SalaryData_Train,SalaryData_Train$Salary)
summary(salary_classifer)


#evaluate on test data
salary_test_pred=predict(salary_classifer,SalaryData_Test)
salary_test_pred
library(gmodels)
CrossTable(salary_test_pred,SalaryData_Test$Salary,
           prop.chisq = F,prop.t = F,prop.r = F,
           dnn = c("predicted","actual"))

accuracy=mean(salary_test_pred==SalaryData_Test$Salary)
accuracy
#we try to increase accuracy by laplace transformation by 1
salary_classifer=naiveBayes(SalaryData_Train,SalaryData_Train$Salary,laplace = 1)
salary_classifer
salary_test_pred=predict(salary_classifer,SalaryData_Test)
CrossTable(salary_test_pred,SalaryData_Test$Salary,
           prop.chisq = F,prop.t = F,prop.r = F,
           dnn = c("predicted","actual"))

accuracy=mean(salary_test_pred==SalaryData_Test$Salary)
accuracy

#we try to increase accuracy by laplace transformation by 2
salary_classifer=naiveBayes(SalaryData_Train,SalaryData_Train$Salary,laplace = 2)
salary_classifer
salary_test_pred=predict(salary_classifer,SalaryData_Test)
CrossTable(salary_test_pred,SalaryData_Test$Salary,
           prop.chisq = F,prop.t = F,prop.r = F,
           dnn = c("predicted","actual"))

accuracy=mean(salary_test_pred==SalaryData_Test$Salary)
accuracy


#laplace 1 gives good accuracy