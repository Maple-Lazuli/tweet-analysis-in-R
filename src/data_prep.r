# define your data dir here (this assumes running from the src directory)
datadir <- "../data/"

# setup the libraries
# Install
install.packages( "tm" )  
install.packages( "plyr" )  
install.packages( "textstem" )  
install.packages( "cld3" )

# Load
library( tm )
library( plyr )
library(textstem)
library( cld3 )

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

# remove  special characters
tweets$tweet <- gsub( url_pattern, "", tweets$tweet ) # strip everything after http(s)
tweets$tweet <- gsub("[^[:alpha:]]+", " ", tweets$tweet ) # remove special characters and numbers
tweets$tweet <- gsub('\\b\\w{1,2}\\b','', tweets$tweet) # remove any word under 3 characters
tweets$tweet <- lemmatize_words(tweets$tweet)

# add columns noting if the tweet text contained a web link and an ssl web link
tweets$link <- grepl( "http", tweets$tweet, fixed = TRUE )
tweets$ssl_link <- grepl( "https", tweets$tweet, fixed = TRUE )

# output dataframe to csv
write.csv( tweets, "tweets_prepped.csv", row.names = FALSE )
