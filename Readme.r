
##multiple regression###
#set the working directory
##trainng data preprocessing 
#Reading the csv file
data<- read.csv("yds_train2018.csv")

#Studying the structure of the variables
str(data)
#Removing the null values.
data<- na.omit(data)
#Removing the redundant columns
data<- data[,-3]
data<- data[,-1]

##Converting catagorical data into factor numbers
data$Country <- factor(data$Country,levels = c("Argentina","Belgium",
                                               "Columbia", "Denmark", "England","Finland") ,
                       labels = c("0", "1", "2", "3","4","5")) 



#testing data preprocessing
newdata<- read.csv("yds_test2018(4) - Copy.csv")
#viewing the dataset
View(data)
#Reading the structure of variables
str(newdata)
newdata<- newdata[,-1]
#Converting catagorical variable into factor numbers
newdata$Country<- factor(newdata$Country, levels = c("Argentina","Belgium",
                                                     "Columbia", "Denmark", "England","Finland") ,
                         labels = c("0", "1", "2", "3","4","5"))

##starting with the regression process

#model for training set
regressor<- lm(formula = Sales ~. , data= data)
summary(regressor)##significant variable


##test set
predict<- predict(regressor, newdata= newdata)
predict#predicitng the values


data1<- data[,5]
View(data1)


###smape function performance
smape_cal <- function(data1, predict){
  data1<- as.numeric(data1)
  predict<- as.numeric(predict)
  smape <- (abs(data1-predict)/(abs(data1)+abs(predict)))
  return(smape)}

smape<- smape_cal(data1, predict)


# Write final submission
library(needs)
1
setwd("C:/Users/new/Desktop/ML/ZS problem/dataset")
write.csv(predict, file = "testfile.csv", row.names = F)
