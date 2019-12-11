#Build a naive bayes model on the data set for classifying the ham and spam
rm(list = ls(all=T))
#install.packages("readr")
library(readr)
sms <- read_csv("C:/Users/Mohd Shaik Atheef/Desktop/lms/data science assignments/1 assignments/naive bayes/sms.csv")
View(sms)


str(sms)
summary(sms)
class(sms)
attach(sms)
sms$type<-as.factor(sms$type)
prop.table(table(sms$type))

#install.packages("tm")
library(tm)

sms_corpous=Corpus(VectorSource(sms$text))

sms_corpous=tm_map(sms_corpous,function(x) iconv(enc2utf8(x), sub = 'byte'))


#cleaning data
corpus_clean=tm_map(sms_corpous,tolower)
corpus_clean=tm_map(sms_corpous,removeNumbers)
corpus_clean=tm_map(sms_corpous,removeWords,stopwords())
corpus_clean=tm_map(corpus_clean,removePunctuation)
removenumpunct=function(x)gsub("[^[:alpha:][:space:]]","",x)
corpus_clean=tm_map(corpus_clean,content_transformer(removenumpunct))
class(corpus_clean)


#create document term matrix sparse
sms_dtm=DocumentTermMatrix(corpus_clean)
class(sms_dtm)
#split data
sms_raw_train=sms[1:4169, ]
sms_raw_test=sms[4170:5559,]

sms_dtm_train=sms_dtm[1:4169,]
sms_dtm_test=sms_dtm[4170:5559,]

sms_corpous_train=corpus_clean[1:4169]
sms_corpous_test=corpus_clean[4170:5559]

prop.table(table(sms_raw_train$type))
prop.table(table(sms_raw_test$type))

#indicator features for frequent words

sms_dist=findFreqTerms(sms_dtm_train,5)

#split data with sms dist
sms_train=DocumentTermMatrix(sms_corpous_train,list(dictionary=sms_dist))
sms_train
sms_test=DocumentTermMatrix(sms_corpous_test,list(dictionary=sms_dist))

conver_counts=function(j){
  j=ifelse(j>0, 1, 0)
  j=factor(j,levels = c(0,1),labels = c("no","yes"))
}
#apply custom define function to train and test
sms_train=apply(sms_train,MARGIN = 2,conver_counts)
sms_test=apply(sms_test,MARGIN = 2,conver_counts)
View(sms_train)

#training a model on the data
#install.packages("e1071")
library(e1071)
sms_classifer=naiveBayes(sms_train,sms_raw_train$type)
sms_classifer

#evaluating model performance
sms_test_pred=predict(sms_classifer,sms_test)
install.packages("gmodels")
library(gmodels)
CrossTable(sms_test_pred,sms_raw_test$type,
           prop.chisq = F,prop.t = F,prop.r = F,
           dnn = c("predicted","actual"))


accuracy=mean(sms_test_pred==sms_raw_test$type)
accuracy
# it is predicting ham as spam that are 4 
#we try to reduce it by laplace
sms_classifer=naiveBayes(sms_train,sms_raw_train$type,laplace = 1)
sms_test_pred=predict(sms_classifer,sms_test)
CrossTable(sms_test_pred,sms_raw_test$type,
           prop.chisq = F,prop.t = F,prop.r = F,
           dnn = c("predicted","actual"))


accuracy=mean(sms_test_pred==sms_raw_test$type)
accuracy
# it is still pedicting ham as spam that are 3
#increase laplace
sms_classifer=naiveBayes(sms_train,sms_raw_train$type,laplace = 2)
sms_test_pred=predict(sms_classifer,sms_test)
CrossTable(sms_test_pred,sms_raw_test$type,
           prop.chisq = F,prop.t = F,prop.r = F,
           dnn = c("predicted","actual"))

#its prediction unchanged
#so go with laplace 1


