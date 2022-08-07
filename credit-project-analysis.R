##################################### SETWD, LIBRARY ###################################
#setwd("C:/Users/Administrator/OneDrive/R/DSA6000/Project")
library(ggplot2)
library(dplyr)
#this is where the KNN function is
require(class) 
library(MASS)
#this is for the confusion matrix function
library(caret)
#pROC draws ROC graphs for us
library(pROC)
#install.packages("gridExtra")
#this is for printing many plots on one 
library(gridExtra)
#reshape2 is required to "melt" the correlation matrix
library(reshape2)

#Using this stuff mostly for the random forests
#install.packages("cowplot")
library(cowplot)
#install.packages("randomForest")
library(randomForest)
#install.packages("party")
library(party)
##################################### DPLYR TUTORIAL ###################################
#import data, assign it to a variable
setwd("~/Desktop/DSA6000/")

credit <- read.csv("credit_data.csv")
#SELECT FUNCTION
#gives all the names of the columns
names(credit)
# SELECT gives the columns specificed
head(dplyr::select(credit, demographic_slice:hold_bal))
#This does a similar thing by selecting everything except for the mentioned columns
head(dplyr::select(credit, -(demographic_slice:hold_bal)))

#FILTER FUNCTION
#allows you to filter dataset based on rules
#filter for credit scores above 700
creditFiltered <- filter(credit, credit_score > 700)
head(creditFiltered)
#filter for credit scores about 700, approved = 0, estimated income > 50,000
creditFiltered <- filter(credit, credit_score > 700 & approved == 0 & est_income > 50,000)
head(creditFiltered)

#ARRANGE FUNCTION
#reorders rows of the dataframe based on the values of the columns
#arrange the dataset by credit score, this does it by lowest to highest
credit <- arrange(credit, credit_score)
#this shows the last 6 rows of the dataset. Notice how they are the highest scores.
tail(credit)
#just use desc if you want it in descending order
credit <- arrange(credit, desc(credit_score))
tail(credit)

#RENAME FUNCTION
#this helps you rename columns in R
#rename demographic_slice to demo 
credit <- rename(credit, demo = demographic_slice)
head(credit)

#MUTATE FUNCTION
#used to create/mutate columns
#create a column credit score trend
credit <- mutate(credit, credit_trend = credit_score-mean(credit_score,na.rm = TRUE))
head(dplyr::select(credit,credit_score,credit_trend))

#GROUPBY FUNCTION
#basically lets you do a pivot of the data
#group the data by demographic
creditByDemo <- group_by(credit,demo)
creditByDemo
#now you can use the summarize function to get specific details
averageCredit <- summarize(creditByDemo, est_income = mean(est_income), hold_bal = mean(hold_bal), credit_score = mean(credit_score))
#you can clean this up using the arrange function!
averageCredit <- arrange(averageCredit,credit_score)
head(averageCredit)


##################################### IMPORT DATA ##################################### 
#import data, assign it to a variable
credit <- read.csv("data/credit_data.csv")
#take a peak at the data
head(credit)
#dimensions of the dataset
dim(credit)
#view the structure of the data
str(credit)
#total of null values
sum(is.na(credit))
#tells us variable types
table(sapply(credit, class))

##################################### Data Analysis ###################################
#at first glance:
# - we have a good distribution of demographics
# - there seems to be a good mean for each column
summary(credit)
# add a column for count
credit <- mutate(credit, count = 1)

#DEMOPGRAHICS
#we see a good distribution of demographics data
demo <- group_by(credit,demographic_slice)
demo <- summarize(demo,total = sum(count))
demoPlot <- ggplot(data=demo,aes(x=demographic_slice, y=total))+
  geom_bar(stat="identity", fill="steelblue")
demoPlot

par(mfrow=c(3,4))

#INCOME
income_plot <-ggplot(credit, aes(x=est_income))+
  geom_histogram(aes(y=..density..), colour="black",fill="white")+
  geom_vline(aes(xintercept=mean(est_income)),
             color="blue",linetype="dashed",size=1)+
  geom_density(alpha=.2,fill="#FF6666")

#HOLD
hold_plot <-ggplot(credit, aes(x=hold_bal))+
  geom_histogram(aes(y=..density..), colour="black",fill="white")+
  geom_vline(aes(xintercept=mean(hold_bal)),
             color="blue",linetype="dashed",size=1)+
  geom_density(alpha=.2,fill="#FF6666")

#Preffered Customer Probablity
custProb_plot <-ggplot(credit, aes(x=pref_cust_prob))+
  geom_histogram(aes(y=..density..), colour="black",fill="white")+
  geom_vline(aes(xintercept=mean(pref_cust_prob)),
             color="blue",linetype="dashed",size=1)+
  geom_density(alpha=.2,fill="#FF6666")

#CREDIT SCORE
credit_plot <-ggplot(credit, aes(x=credit_score))+
  geom_histogram(aes(y=..density..), colour="black",fill="white")+
  geom_vline(aes(xintercept=mean(credit_score)),
             color="blue",linetype="dashed",size=1)+
  geom_density(alpha=.2,fill="#FF6666")

#RISK SCORE
risk_plot <-ggplot(credit, aes(x=risk_score))+
  geom_histogram(aes(y=..density..), colour="black",fill="white")+
  geom_vline(aes(xintercept=mean(risk_score)),
             color="blue",linetype="dashed",size=1)+
  geom_density(alpha=.2,fill="#FF6666")

#CREDIT EVALUATION
creditEval_plot <-ggplot(credit, aes(x=imp_crediteval))+
  geom_histogram(aes(y=..density..), colour="black",fill="white")+
  geom_vline(aes(xintercept=mean(imp_crediteval)),
             color="blue",linetype="dashed",size=1)+
  geom_density(alpha=.2,fill="#FF6666")

#AXIO SCORE
axio_Plot <-ggplot(credit, aes(x=axio_score))+
  geom_histogram(aes(y=..density..), colour="black",fill="white")+
  geom_vline(aes(xintercept=mean(axio_score)),
             color="blue",linetype="dashed",size=1)+
  geom_density(alpha=.2,fill="#FF6666")

#Using the gridExtra library to plot all the histograms on one
grid.arrange(income_plot,hold_plot,custProb_plot,credit_plot,risk_plot,creditEval_plot,axio_Plot, ncol=3)


#APPROVED
#only 15% of applicants get approved!
approved <- group_by(credit,approved)
approved <- summarize(approved, total = sum(count))
approved <- mutate(approved, percentage = total/sum(total))
approved

approvedPlot <- ggplot(approved, aes(x="", y=total,fill=approved))+
  geom_bar(width=1, stat="identity")+
  coord_polar("y",start=0)
approvedPlot

#CORRELATION MATRIX
creditNumericData <- dplyr::select(credit,est_income:approved)
cormat <- round(cor(creditNumericData),2)
head(cormat)

melted_cormat <- melt(cormat)
head(melted_cormat)
ggplot(data=melted_cormat, aes(x=Var1, y=Var2, fill=value))+
  geom_tile()
#cleaning the correlation matrix
# Get lower triangle of the correlation matrix
get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}
# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}

upper_tri <- get_upper_tri(cormat)
upper_tri

#We do not see any negative correlations
#approved, positively correlated with the following:
# - pref_cust_prob
# - est_income
# - hold_bal
melted_cormat <- melt(upper_tri, na.rm = TRUE)
ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "grey", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()

##################################### DATA SETUP FOR PREDICTING ###################################
#create a train and test dataset
#create a value for dividing the data into train and test by 80% of the number of rows in the data
size <- floor(0.80*nrow(credit))
#setting seed ensures we have randomness
set.seed(123)
#randomly indentifies the rows equal to the sample size
train_indicator <- sample(seq_len(nrow(credit)),size = size)
trainCredit <- credit[train_indicator,]
testCredit <- credit[-train_indicator,]


#80% of random data
dim(trainCredit)
#20% of random data
dim(testCredit)

##################################### LOGISTIC REGRESSION #######################################
#using logistic regression to predict approval 
#removed a few columns because their p-value was too high
logisticRegressionModel <- glm(approved ~. -risk_score -imp_crediteval -axio_score -count , family = binomial,data=trainCredit)
summary(logisticRegressionModel)

logRegression_TestCreditResults <- mutate(testCredit, approvalPrediction = round(predict(logisticRegressionModel,testCredit,type="response"),digits = 0),
                            approvalProbability = predict(logisticRegressionModel,testCredit,type="response"))
head(logRegression_TestCreditResults)

#confusion matrix to show results
logRegressionResults <- table(logRegression_TestCreditResults$approved,logRegression_TestCreditResults$approvalPrediction)
#confusion matrix of predictions
#accuracy rate of 95.2%
print(confusionMatrix(logRegressionResults))


#find the errors
logRegression_WrongPredictions <- filter(logRegression_TestCreditResults, approved != approvalPrediction)
count(logRegression_WrongPredictions)


#logistic regression curve
#sort the predicted data
logRegression_TestCreditResults <- arrange(logRegression_TestCreditResults, approvalProbability)
head(logRegression_TestCreditResults)

#add a rank to each sample, from low probability to high probability
logRegression_TestCreditResults <- mutate(logRegression_TestCreditResults,rank=1:nrow(logRegression_TestCreditResults))
head(logRegression_TestCreditResults)

#plot logistic regression line
ggplot(data=logRegression_TestCreditResults, aes(x=rank, y=approvalProbability))+
  geom_point(aes(color=approved), alpha=1, shape=4, stroke=2)+
  xlab("Index")+
  ylab("Predicted Probability of Approval")


##################################### LINEAR DISCIMINANT ANALYSIS ###################################
#LDA model, using trainCredit column 1-9 to get rid of count column
creditLDA <- lda(approved~ ., trainCredit[1:9])
#As we have seen in the past, 85% are declined, 15% are approved.
#average approved income is $86,161
#average credit score is 669
creditLDA

#Predict credit approval with LDA
ldaPredictions <- predict(creditLDA, testCredit)
ldaPredictions

#1696 correct predicitons, #304 incorrect predictions
summary(ldaPredictions$class)

#reset margins for plot
par(mar=c(1,1,1,1))
#Show histogram of predictions
ldahist(data=ldaPredictions$x[,1], g=testCredit$approved)

#confusion matrix of predictions
#accuracy rate of 93.8%
ldaPredictionsDF <- data.frame(ldaPredictions$class, testCredit$approved)
ldaResults <- table(ldaPredictionsDF$ldaPredictions.class,ldaPredictionsDF$testCredit.approved)
print(confusionMatrix(ldaResults))

##################################### RANDOM FOREST ###################################
#Setting approved to be classification friendly for test and train

randomForest_Credit <- randomForest(approved ~., data=trainCredit,ntree=10,proximity=TRUE)
randomForest_Credit$importance

rfPredictions <-predict(randomForest_Credit,testCredit)
rfPredictionsDF <-data.frame(rfPredictions,testCredit$approved)



##################################### KNN ###################################

#KNN requries the following inputs: training data, test data, training target, and K
# K should be the sqtr of the amount of rows you have in your data. You should also use an odd number so it can take a majority vote
#sqrt = 89
sqrt(nrow(trainCredit))

#KNN mode only accepts numeric values. Here we are making new train and test data sets with numeric columns only!
knnTrainCredit <- dplyr::select(trainCredit,est_income:approved)
knnTestCredit <- dplyr::select(testCredit,est_income:approved)
#build the knn model
knnModel <- knn(train=knnTrainCredit,test=knnTestCredit,cl=knnTrainCredit$approved,k=15)

knnTestPlusModel <- data.frame(knnTestCredit$approved,knnModel)

knnResults <- table(knnTestCredit$approved,knnModel)

#Print the confusionmatrix. We see 85% accuracy
print(confusionMatrix(knnResults))


##################################### ROC & AUC CURVE ###################################

#resets the ploting to show 1
par(mar=c(1,1,1,1))

#PLOT LOGISTIC REGRESSION
par(pty ="s") #this gets rid of the padding on the side of the graph
#plot the logistic regression results
#We see that the AUC for Logistic Regression = 98.31%
roc(logRegression_TestCreditResults$approved,logRegression_TestCreditResults$approvalProbability, plot = TRUE, legacy.axes=TRUE, percent = TRUE,
    xlab = "False Positive Percentage", ylab="True Positive Percentage",
    col="#377eb8",lwd=4, print.auc=TRUE, print.auc.x=90, print.auc.y = 80,
    auc.polygon=TRUE, auc.polygon.col="#377eb822")


#PLOT LDA
#We see that the AUC for LDA = 97.8%
plot.roc(testCredit$approved, as.numeric(ldaPredictions$x), percent=TRUE,col="#4daf4a",lwd=4,
         print.auc=TRUE, add=TRUE, print.auc.x=90, print.auc.y=70)

#PLOT RANDOM FOREST
plot.roc(testCredit$approved,rfPredictionsDF$rfPredictions, percent=TRUE, col="red",lwd=4,
         print.auc=TRUE, add=TRUE,print.auc.x=90, print.auc.y = 90)


#PLOT KNN
plot.roc(knnTestCredit$approved,as.numeric(knnModel), percent=TRUE, col="orange",lwd=4,
         print.auc=TRUE, add=TRUE, print.auc.x=90,print.auc.y=60)

#set the legend
legend("bottomright", legend = c("Random Forest","Logistic Regression","LDA","KNN"),
       col=c("red","blue", "green","orange"),lwd = 4)











