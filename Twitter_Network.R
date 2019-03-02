## Download the data and put in a sqlite database

#There are 113 `tar.gz` files in the folder. In this section, you are asked to process all files into the data set. There can be several strategies, such as using the methods you have already learned in the previous assignments (c.f. `parseTweets`), or you can try an R-package **readtext**. 

### Create a database

#install.packages('dbplyr')
#install.packages('DBI')
#install.packages('readtext')
#install.packages('streamR')
#install.packages('R.utils')
#install.packages('rjson')
#install.packages('ldply')
#install.packages('plyr')
#install.packages('gtools')
#install.packages('RSQLite')
#install.packages('quanteda')
#install.packages('ggplot2')
#install.packages('data.table')
install.packages("devtools")
#install.packages('gtools')
install_github("ChandlerLutz/crossplotr")


library(quanteda)
library(readtext)
library(streamR)
library(R.utils)
library(plyr)
library(gtools)
library("RSQLite")
library(twitteR) ### for fetching the tweets
library(plyr) ## for breaking the data into manageable pieces
library(ROAuth) # for R authentication
library(stringr) # for string processing
library(ggplot2) # for plotting the results
library(tm)
library(wordcloud)

reqURL<- "https://api.twitter.com/oauth/request_token"
accURL<- "https://api.twitter.com/oauth/access_token"
authURL<- "https://api.twitter.com/oauth/authorize"

CK <- "xxxxxx"
CS <- "xxxxxx"

AT <- "xxxxxxx"
ATS <-  "xxxxxxx" 

setup_twitter_oauth(CK,CS, AT, ATS)

all_hash <- c("#Myntra","@MyntraSupport","#myntra","@myntra","#jabong","@JabongIndia" , "#AJio","@AJIOLife","#ajio","#AJIO","@AJio", "#shein","@SheIn_official","#sheingals", "#sheinstyle", "#like", "#model", "#beautiful", "#happy", "#mensfashion", "#trendy", "#winter", "#fashionmodel", "#modeling","#cold" ,"#fashionstyle")



all_hash_tags <- paste(all_hash,collapse = " OR ")

all_hash_tags

# Document Feature Matrix
tweet_dfm <- dfm(all_hash_tags, remove_punct = TRUE)
head(tweet_dfm)

#Extract most common hashtags
tag_dfm <- dfm_select(tweet_dfm, pattern = ("#*"))
toptag <- names(topfeatures(tag_dfm, 25))
head(toptag)

#Construct feature-occurrence matrix of hashtags
tag_fcm <- fcm(tag_dfm)
head(tag_fcm)

topgat_fcm <- fcm_select(tag_fcm, pattern = toptag)
textplot_network(topgat_fcm, min_freq = 0.1, edge_alpha = 0.8, edge_size = 1)

