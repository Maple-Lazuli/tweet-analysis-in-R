library(e1071)
library(tidyverse)
library(ROCR)
library(dplyr)
library(stringr)
library(tidytext)
library(forcats)
library(textdata)
library("tm")
library("arules")

# set working directory
setwd("F:/Project")

#Upload file into data frame and explore structure
tweets_naive <- read.csv("enriched_tweet_data.csv", sep =",", header = TRUE)
str(tweets_naive)
glimpse(tweets_naive)


# Create the corpus
tweets_naive_corpus <- Corpus(VectorSource(tweets_naive$tweet))

# create the DTM
tweets_naive_dtm <- DocumentTermMatrix(tweets_naive_corpus)

# cast the DTM to a useful format
tweets_naive_dtm <- as.matrix(tweets_naive_dtm)


# Cast the link fields to an integer
tweets_naive_1 <- tweets_naive %>% mutate(link = as.integer(link)) %>% mutate(ssl_link = as.integer(ssl_link))

# EDA showed us that the character length seemed to matter. Lets add a check to see if the tweet is larger or smaller than the median.
median_length <- median((tweets_naive_1 %>% mutate(nchar = nchar(tweet)))$nchar)
tweets_naive_1 <- tweets_naive %>% mutate(larger_than_median = nchar(tweet) > median_length) %>% mutate(larger_than_median = as.integer(larger_than_median))


#convert attributes to factors and characterize columns with presence or absence of the attribute value
tweets_naive_1$anger<-ifelse(tweets_naive_1$anger>0, 1, tweets_naive_1$anger)
tweets_naive_1$anticipation<-ifelse(tweets_naive_1$anticipation>0, 1, tweets_naive_1$anticipation)
tweets_naive_1$disgust<-ifelse(tweets_naive_1$disgust>0, 1, tweets_naive_1$disgust)
tweets_naive_1$fear<-ifelse(tweets_naive_1$fear>0, 1, tweets_naive_1$fear)
tweets_naive_1$joy<-ifelse(tweets_naive_1$joy>0, 1, tweets_naive_1$joy)
tweets_naive_1$negative<-ifelse(tweets_naive_1$negative>0, 1, tweets_naive_1$negative)
tweets_naive_1$positive<-ifelse(tweets_naive_1$positive>0, 1, tweets_naive_1$positive)
tweets_naive_1$sadness<-ifelse(tweets_naive_1$sadness>0, 1, tweets_naive_1$sadness)
tweets_naive_1$surprise<-ifelse(tweets_naive_1$surprise>0, 1, tweets_naive_1$surprise)
tweets_naive_1$trust<-ifelse(tweets_naive_1$trust>0, 1, tweets_naive_1$trust)
tweets_naive_1$sentiment_afinn<-discretize(tweets_naive_1$sentiment_afinn, method="frequency", breaks = 4)

tweets_naive_1$ssl_link <- NULL
tweets_naive_1$tweet_orig<-NULL
tweets_naive_1$length<-NULL
tweets_naive_1$tweet<-NULL
tweets_naive_1$hashtag<-NULL
tweets_naive_1$sentiment_afinn<-NULL  #Is not a completely independent variable
tweets_naive_1$http<-NULL #Contains too many missing values 

glimpse(tweets_naive_1)


############## Training #########################


# Set the seed value to ensure that result is reproducible
set.seed(1234)
# divide the data into training and test
ind <- sample(2, nrow(tweets_naive_1), replace = TRUE, prob = c(0.7, 0.3))
train.data <- tweets_naive_1 [ind == 1, ]
test.data <- tweets_naive_1 [ind == 2, ]

# Using naive bayes model
model<-naiveBayes(label~., train.data)
model
prop.table(table(train.data$label))  # Returns the same output as A-priori probabilitys in the model


################ Evaluation #####################


# Evaluate the model on a training set
# This command returns the predicted probabilities for each class
predict(model, train.data, type="raw")

# This command returns the predicted class
predict(model, train.data, type="class") 
table(predict(model, train.data, type="class"), train.data$label, dnn=c("predicted", "actual"))

# classification accuracy for the training data
mean(predict (model, train.data, type="class")== train.data$label)

# Classification error for the training data
mean(predict (model, train.data, type="class")!= train.data$label)

#  Evaluate the model on a test set
predict(model, test.data, type="raw")

# This command returns the predicted probabilities for each class
predict(model, test.data, type="class") 
# This command returns the predicted class
table(predict(model, test.data, type="class"), test.data$label, dnn=c("predicted", "actual"))
# classification accuracy for the training data
mean(predict (model, test.data, type="class")== test.data$label)
# Classification error for the training data
mean(predict (model, test.data, type="class")!= test.data$label)


mypredictions<-predict(model, test.data, type="class")
ROCRpred <- prediction(as.numeric(mypredictions)-1, test.data$label)
ROCRperf <- performance(ROCRpred, 'tpr','fpr')
plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2,1.2), lwd=2)
abline(a=0,b=1,lwd=2,lty=2,col="gray")

## Mosaic Plot ##
#mosaic plot is a special type of stacked bar chart that shows percentages of data in groups
mosaicplot(table(predict(model, test.data), test.data$label), shade = TRUE, main = "Predicted vs. Actual LABEL")