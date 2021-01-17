###  Aditi Chhabria  ###

#### TASK 1 ####
# Prediction Using Supervised ML #

# Import necessary libraries 

library(caret)
library(corrplot)
library(lmtest)

#step1: import data

data=readxl::read_xlsx( "C:\\Users\\aditi\\Desktop\\RStudio\\task#1_TSF,JAN2021 (1).xlsx")
View(data)
summary(data)

#step2 : Partitioning the dataset

set.seed(123)
pt=createDataPartition(data$Scores,p=0.7,list = F,groups = 2)
data.tr=data[pt,]
data.te=data[-pt,]

#step3: check correlation 

library(corrplot)
c=cor(data.tr);c

#step4: plot a scatterplot to see the spread of data
plot(data.tr$Hours,data.tr$Scores,pch=20)

#step5: Create simple linear regression model
#Here our y=scores and x=hours

attach(data.tr)
model=lm(Scores~Hours)
summary(model)

abline(model, col="magenta")

#step6: Test the model
data.te$predicted_scores=predict(object=model,newdata=data.te);data.te

#check the prediction error rate
RMSE(data.te$predicted_scores, data.te$Scores)/mean(data.te$Scores)

#if the student studies for 9.25hrs/day 
y=1.6125+10.1670*9.25;y

# The predicted score of the student is 95.65%