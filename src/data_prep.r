# define your data dir here (this assumes running from the src directory)
datadir <- "../data/"

# setup the libraries
# Install
#install.packages( "tm" )  
#install.packages( "plyr" )  
#install.packages( "textstem" )  
#install.packages( "cld3" )
#install.packages( "qdapRegex" )

# Load
library( tm )
library( plyr )
library(textstem)
library( cld3 )
library( qdapRegex )

# set the working directory where your data is
setwd( datadir )

# read in the training data set
tweets_train <- read.csv( "Constraint_Train.csv", header=TRUE, sep=",", as.is=FALSE )
tweets_test <- read.csv( "english_test_with_labels.csv", header=TRUE, sep=",", as.is=FALSE )
tweets <- rbind( tweets_train, tweets_test )

# using the below function, we learn that there are 6420 observations with 
# three columns:
# 
# id is of type int
# tweets is of type factor with 6420  levels
# label is a factor with 2 levels (fake, real)
str( tweets )

# convert tweet from factor to character type
tweets$tweet <- as.character( tweets$tweet )

# remove the id field
tweets$id <- NULL

# check for missing values.  if the sum of all of the missing values is zero, 
# there are no missing values.  this dataset produces a sum of zero, 
# so complete data with no missing data
sum( is.na( tweets ) )

# some summary info...  fake tweets: 3060, real tweets: 3360
summary( tweets$label )

# prep the data

# to lower case
tweets$tweet <- tolower( tweets$tweet )

# remove stop words
tweets$tweet <- gsub(paste0('\\b',tm::stopwords("english"), '\\b', collapse = '|'), '', tweets$tweet)

# url pattern for removal
url_pattern <- "://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+"

# remove urls
tweets$tweet <- gsub( url_pattern, "", tweets$tweet ) # strip everything after http(s)

# remove hashtags and save to new field
tweets$tweet_orig <- tweets$tweet
tweets$tweet <- rm_hash( tweets$tweet_orig )
tweets$hashtag <- ex_hash( tweets$tweet_orig )

# remove  special characters
tweets$tweet <- gsub("[^[:alpha:]]+", " ", tweets$tweet ) 
tweets$tweet_orig <- gsub("[^[:alpha:]]+", " ", tweets$tweet_orig )
tweets$hashtag <- gsub("[^[:alpha:]]+", " ", tweets$hashtag )

# remove any word under 3 characters 
# (hashtag is included to remove 'c' that remained when the list was converted to char data)
tweets$tweet <- gsub('\\b\\w{1,2}\\b','', tweets$tweet) 
tweets$tweet_orig <- gsub('\\b\\w{1,2}\\b','', tweets$tweet_orig) 
tweets$hashtag <- gsub('\\b\\w{1,2}\\b','', tweets$hashtag) 

#stem the words (not the hashtags)
tweets$tweet <- lemmatize_words(tweets$tweet)
tweets$tweet_orig <- lemmatize_words(tweets$tweet_orig)

# add columns noting if the tweet text contained a web link and an ssl web link
tweets$link <- grepl( "http", tweets$tweet, fixed = TRUE )
tweets$ssl_link <- grepl( "https", tweets$tweet, fixed = TRUE )

# output dataframe to csv
write.csv( tweets, "tweets_prepped.csv", row.names = FALSE )

# remove http and https terms from the tweets
tweets$tweet <- gsub('\\b\\http[s]?\\b','', tweets$tweet)
tweets$tweet_orig <- gsub('\\b\\http[s]?\\b','', tweets$tweet_orig)

# output dataframe to csv
write.csv( tweets, "tweets_prepped_no_http.csv", row.names = FALSE )
