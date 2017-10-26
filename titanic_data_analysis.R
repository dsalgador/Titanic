setwd("C:/Users/Daniel/Dropbox/GitHub/Titanic")
############################
#The data cleaning process
#############################

training.data.raw <- read.csv('train.csv',header=T,na.strings=c(""))
#Make sure that the parameter na.strings is equal to c("") so that each missing value is coded as a NA. 

#Now we need to check for missing values and look how many unique values there are for each variable :
sapply(training.data.raw,function(x) sum(is.na(x)))
#sapply() applies function f to each column of the data frame
sapply(training.data.raw, function(x) length(unique(x)))

#To visualize missing values of a dataset:
library(Amelia)
#install.packages("Amelia")
missmap(training.data.raw, main = "Missing values vs observed")

#The variable cabin has too many missing values, we will not use it. 
#We will also drop PassengerId since it is only an index and Ticket.

data <- subset(training.data.raw,select=c(2,3,5,6,7,8,10,12))
###################################
#Taking care of the missing values
###################################

#as an approximation we substitute NA values
#by the means of the observation values that are not NA
data$Age[is.na(data$Age)] <- mean(data$Age,na.rm=T) #na.rm = T, does not take into account NA values to compute the mean

is.factor(data$Sex)
is.factor(data$Embarked)

contrasts(data$Sex)
contrasts(data$Embarked)
#As for the missing values in Embarked, since there are only two, we will discard those two rows (we could also have replaced the missing values with the mode and keep the datapoints).

data <- data[!is.na(data$Embarked),]
rownames(data) <- NULL



########################
#Model fitting
#########################
train <- data[1:800,]
test <- data[801:889,]


model <- glm(Survived ~.,family=binomial(link='logit'),data=train)
summary(model)

#########################
#Interpreting the results of our logistic regression model
###############################

#we can run the anova() function on the model to analyze the table of deviance
anova(model, test="Chisq")
#A large p-value here indicates that the model without the variable explains more or less the same amount of variation. 
#install.packages("pscl")
library(pscl)

#While no exact equivalent to the R2 of linear regression exists, the McFadden R2 index can be used to assess the model fit.
pR2(model)


#########################
#Assessing the predictive ability of the model
############################

fitted.results <- predict(model,newdata=subset(test,select=c(2,3,4,5,6,7,8)),type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)

misClasificError <- mean(fitted.results != test$Survived) #proportion of failures 
print(paste('Accuracy',1-misClasificError)) #proportion of right predictions


#install.packages("ROCR")
library(ROCR)
p <- predict(model, newdata=subset(test,select=c(2,3,4,5,6,7,8)), type="response")
pr <- prediction(p, test$Survived)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc
