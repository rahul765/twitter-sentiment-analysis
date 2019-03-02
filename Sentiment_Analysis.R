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

CK <- "xxxxxxx"
CS <- "xxxxxxx"
AT <- "xxxxxxx"
ATS <-  "xxxxxx" 

setup_twitter_oauth(CK,CS, AT, ATS)

hash_Myntra <- c("#Myntra","@MyntraSupport","#myntra","@myntra")
hash_Jab <- c("@JabongIndia" , "#jabong")
hash_Ajio <- c("#AJio","@AJIOLife","#ajio","#AJIO","@AJio")
hash_Shein <- c("#shein","@SheIn_official","#sheingals", "#sheinstyle")



myntra_tags <- paste(hash_Myntra,collapse = " OR ")
jab_tags <- paste(hash_Jab,collapse = " OR ")
ajio_tags <- paste(hash_Ajio,collapse = " OR ")
shein_tags <- paste(hash_Shein,collapse = " OR ")

myntra_tags
jab_tags
ajio_tags
shein_tags


#myntra tweet search & cloud


myntra_Tweet <- searchTwitter(myntra_tags,n=5000,lang = "en")
myntra_Data <- twListToDF(myntra_Tweet)
myntraTextData <- myntra_Data$text
View(myntraTextData)
myntraTweets <- gsub("http:*","",myntraTextData)
myntraTweets <- gsub("@\\S+","",myntraTweets)
myntraTweets <- gsub("[ |\t]{2,}","",myntraTweets)
myntraTweets <- gsub("[[:punct:]]","",myntraTweets)
myntraTweets <- gsub("RT","",myntraTweets)
myntraTweets <- iconv(myntraTweets,from = "Utf8",to = "ASCII", sub="")
myntraTweets <- gsub("[[:digit:]]+","",myntraTweets)

length(myntraTweets)

myntraTweets <- myntraTweets[!duplicated(myntraTweets)]

length(myntraTweets)

View(myntraTweets)


tweets_myntra <- Corpus(VectorSource(myntraTweets))
tweets_myntra <- tm_map(tweets_myntra,tolower)
tweets_myntra <- tm_map(tweets_myntra,removeWords,stopwords("en"))

dtm_myntra <- DocumentTermMatrix(tweets_myntra)
mat_myntra <- as.matrix(dtm_myntra) 
freq_dtm_myntra <- colSums(mat_myntra)
length(freq_dtm_myntra)
ord_myntra <- order(freq_dtm_myntra,decreasing = TRUE)
View(freq_dtm_myntra[ord_myntra])


WC <- wordcloud(words= names(freq_dtm_myntra), freq = freq_dtm_myntra, min.freq = 3, random.order = FALSE)


#-------------------------jabong---------------------------------------------#


jab_Tweet <- searchTwitter(jab_tags,n=5000,lang = "en")
jab_Data <- twListToDF(jab_Tweet)
jabTextData <- jab_Data$text
View(jabTextData)
jabTweets <- gsub("http:*","",jabTextData)
jabTweets <- gsub("@\\S+","",jabTweets)
jabTweets <- gsub("[ |\t]{2,}","",jabTweets)
jabTweets <- gsub("[[:punct:]]","",jabTweets)
jabTweets <- gsub("RT","",jabTweets)
jabTweets <- iconv(jabTweets,from = "Utf8",to = "ASCII", sub="")
jabTweets <- gsub("[[:digit:]]+","",jabTweets)


length(jabTweets)
jabTweets <- jabTweets[!duplicated(jabTweets)]
length(jabTweets)

View(jabTweets)


tweets_jab <- Corpus(VectorSource(jabTweets))
tweets_jab <- tm_map(tweets_jab,tolower)
tweets_jab <- tm_map(tweets_jab,removeWords,stopwords("en"))

dtm_jab <- DocumentTermMatrix(tweets_jab)
mat_jab <- as.matrix(dtm_jab) 

freq_dtm_jab <- colSums(mat_jab)
length(freq_dtm_jab)
ord_jab <- order(freq_dtm_jab,decreasing = TRUE)
View(freq_dtm_jab[ord_jab])

WC_jab <- wordcloud(words= names(freq_dtm_jab), freq = freq_dtm_jab, min.freq = 3, random.order = FALSE)



#--------------------------------------------AJIO---------------------------------#

ajio_Tweet <- searchTwitter(ajio_tags,n=5000,lang = "en")
ajio_Data <- twListToDF(ajio_Tweet)
ajioTextData <- ajio_Data$text
ajioTweets <- gsub("http:*","",ajioTextData)
ajioTweets <- gsub("@\\S+","",ajioTweets)
ajioTweets <- gsub("[ |\t]{2,}","",ajioTweets)
ajioTweets <- gsub("[[:punct:]]","",ajioTweets)
ajioTweets <- gsub("RT","",ajioTweets)
ajioTweets <- iconv(ajioTweets,from = "Utf8",to = "ASCII", sub="")
ajioTweets <- gsub("[[:digit:]]+","",ajioTweets)


length(ajioTweets)
ajioTweets <- ajioTweets[!duplicated(ajioTweets)]
length(ajioTweets)

View(ajioTweets)



tweets_ajio <- Corpus(VectorSource(ajioTweets))
tweets_ajio <- tm_map(tweets_ajio,tolower)
tweets_ajio <- tm_map(tweets_ajio,removeWords,stopwords("en"))

dtm_ajio <- DocumentTermMatrix(tweets_ajio)
mat_ajio <- as.matrix(dtm_ajio) 
freq_dtm_ajio <- colSums(mat_ajio)
length(freq_dtm_ajio)
ord_ajio <- order(freq_dtm_ajio,decreasing = TRUE)
View(freq_dtm_ajio[ord_ajio])

library(wordcloud)
WC_ajio <- wordcloud(words= names(freq_dtm_ajio), freq = freq_dtm_ajio, min.freq = 3, random.order = FALSE)


#---------------------------------------------SHEIN----------------------------#

shein_Tweet <- searchTwitter(shein_tags,n=5000,lang = "en")
shein_Data <- twListToDF(shein_Tweet)
sheinTextData <- shein_Data$text

sheinTweets <- gsub("http:*","",sheinTextData)
sheinTweets <- gsub("@\\S+","",sheinTweets)
sheinTweets <- gsub("[ |\t]{2,}","",sheinTweets)
sheinTweets <- gsub("[[:punct:]]","",sheinTweets)
sheinTweets <- gsub("RT","",sheinTweets)
sheinTweets <- iconv(sheinTweets,from = "Utf8",to = "ASCII", sub="")
sheinTweets <- gsub("[[:digit:]]+","",sheinTweets)


length(sheinTweets)
sheinTweets <- sheinTweets[!duplicated(sheinTweets)]
length(sheinTweets)

tweets_shein <- Corpus(VectorSource(sheinTweets))
tweets_shein <- tm_map(tweets_shein,tolower)
tweets_shein <- tm_map(tweets_shein,removeWords,stopwords("en"))

dtm_shein <- DocumentTermMatrix(tweets_shein)
mat_shein <- as.matrix(dtm_shein) 
freq_dtm_shein <- colSums(mat_shein)
length(freq_dtm_shein)
ord_shein <- order(freq_dtm_shein,decreasing = TRUE)
View(freq_dtm_shein[ord_shein])

library(wordcloud)
WC_shein <- wordcloud(words= names(freq_dtm_shein), freq = freq_dtm_shein, min.freq = 3, random.order = FALSE)

#-------------------------Wordcloud2------------------

library(dplyr)
library(wordcloud2)
library(tibble)

freq_df <- tibble(words=names(freq_dtm_shein),value=freq_dtm_shein)
freq_df <- freq_df %>% arrange(desc(value))
freq_df
wc2 <- wordcloud2(data = freq_df, size = 1,shape = "star")
wc2


#------------------------------Sentimental Analysis------------------#


posText <- read.delim("positive-words.txt",header = FALSE, stringsAsFactors = FALSE)
posText <- posText$V1
posText <- unlist(lapply(posText,function(x){str_split(x,"\n")}))

negText <- read.delim("negative-words.txt",header = FALSE, stringsAsFactors = FALSE)
negText <- negText$V1
negText <- unlist(lapply(negText,function(x){str_split(x,"\n")}))

myntra_txt = sapply(myntra_Tweet, function(t){t$getText()})
jab_txt = sapply(jab_Tweet, function(t){t$getText()})
ajio_txt = sapply(ajio_Tweet, function(t){t$getText()})
shein_txt = sapply(shein_Tweet, function(t){t$getText()})

noOfTweets <- c(length(myntra_txt),length(jab_txt),length(ajio_txt),length(shein_txt))

ecomlist <- c(myntra_txt,jab_txt,ajio_txt,shein_txt)
source("Sentiment Score.R")
scores = score.sentiment(ecomlist,posText,negText,.progress='text' )
scores$ecom= factor(rep(c("Myntra","Jabong","Ajio","Shein"),noOfTweets))

scores$positive <- as.numeric(scores$score>0)
scores$negative <- as.numeric(scores$score<0)
scores$neutral <- as.numeric(scores$score==0)

myntra_ecom <- subset(scores,scores$ecom=="Myntra")
jab_ecom <- subset(scores,scores$ecom=="Jabong")
ajio_ecom <- subset(scores,scores$ecom=="Ajio")
shein_ecom <- subset(scores,scores$ecom=="Shein")


myntra_ecom$polarity <- ifelse(myntra_ecom$score>0,"Positive",ifelse(myntra_ecom$score<0,"Negative",ifelse(myntra_ecom$score==0,"Neutral",0)))
jab_ecom$polarity <- ifelse(jab_ecom$score>0,"Positive",ifelse(jab_ecom$score<0,"Negative",ifelse(jab_ecom$score==0,"Neutral",0)))
ajio_ecom$polarity <- ifelse(ajio_ecom$score>0,"Positive",ifelse(ajio_ecom$score<0,"Negative",ifelse(ajio_ecom$score==0,"Neutral",0)))
shein_ecom$polarity <- ifelse(shein_ecom$score>0,"Positive",ifelse(shein_ecom$score<0,"Negative",ifelse(shein_ecom$score==0,"Neutral",0)))

qplot(factor(polarity),data=myntra_ecom,geom = "bar",fill=factor(polarity))+xlab("Polarity Categories")+ylab("Frequency")+ggtitle("Customer Sentiments - Myntra")
qplot(factor(score),data=myntra_ecom,geom = "bar",fill=factor(score))+xlab("Sentiment Score")+ylab("Frequency")+ggtitle("Customer Sentiments Scores - Myntra")

qplot(factor(polarity),data=jab_ecom,geom = "bar",fill=factor(polarity))+xlab("Polarity Categories")+ylab("Frequency")+ggtitle("Customer Sentiments - Jabong")
qplot(factor(score),data=jab_ecom,geom = "bar",fill=factor(score))+xlab("Sentiment Score")+ylab("Frequency")+ggtitle("Customer Sentiments Scores - Jabong")

qplot(factor(polarity),data=ajio_ecom,geom = "bar",fill=factor(polarity))+xlab("Polarity Categories")+ylab("Frequency")+ggtitle("Customer Sentiments - Ajio")
qplot(factor(score),data=ajio_ecom,geom = "bar",fill=factor(score))+xlab("Sentiment Score")+ylab("Frequency")+ggtitle("Customer Sentiments Scores - Ajio")

qplot(factor(polarity),data=shein_ecom,geom = "bar",fill=factor(polarity))+xlab("Polarity Categories")+ylab("Frequency")+ggtitle("Customer Sentiments - Shein")
qplot(factor(score),data=shein_ecom,geom = "bar",fill=factor(score))+xlab("Sentiment Score")+ylab("Frequency")+ggtitle("Customer Sentiments Scores - Shein")


df= ddply(scores,c("ecom"),summarise,pos_count=sum(positive),neg_count=sum(negative),neu_count=sum(neutral))

df$total_count = df$pos_count+df$neg_count+df$neu_count

df$pos_percent = round(100*df$pos_count/df$total_count)
df$neg_percent = round(100*df$neg_count/df$total_count)
df$neu_percent = round(100*df$neu_count/df$total_count)

attach(df)

lbls <- paste(df$ecom,df$pos_percent)
lbls <- paste(lbls,"%",sep = "")
pie(pos_percent,labels = lbls,col = rainbow(length(lbls)),main = "Positive Comparative Analysis - E-Commerce")

lbls <- paste(df$ecom,df$neg_percent)
lbls <- paste(lbls,"%",sep = "")
pie(neg_percent,labels = lbls,col = rainbow(length(lbls)),main = "Negative Comparative Analysis - E-Commerce")

lbls <- paste(df$ecom,df$neu_percent)
lbls <- paste(lbls,"%",sep = "")
pie(neu_percent,labels = lbls,col = rainbow(length(lbls)),main = "Neutral Comparative Analysis - E-Commerce")


