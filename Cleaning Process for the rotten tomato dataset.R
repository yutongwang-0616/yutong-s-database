#library basic packages
library(ggplot2)
library(lattice)
library(dplyr)
library(tm)
library(readr)
library(tidyr)
library(data.table)
library(stringr)
library(dummies)
library(tidytext)

#install dataset
setwd("D:/Õı”ÔÕ©/2020 Spring/5205 R/Assignments/Project 1")
movie=read_csv('rotten_tomatoes_movies_raw dataset.csv')
glimpse(movie)

#remove useless variables
movie=select(movie,-rotten_tomatoes_link,-poster_image_url,-in_theaters_date,-on_streaming_date)

#remove Null
movie=na.omit(movie) 

#ramdon sampling
library(caTools)
set.seed(101)
split = sample.split(Y = movie$tomatometer_status, SplitRatio = 0.2)
train=movie[split,]
table(train$tomatometer_status)

#extract 3 main casts
cast1=strsplit(train$cast, split = ", ")
DF <- structure(cast1,row.names = c(NA,-3), class = "data.frame")
DF_transpose<-transpose(DF)
tags_c <- c('cast1','cast2','cast3')
colnames(DF_transpose)<-tags_c
cast=DF_transpose[1:3]
train_clean=cbind(train,cast)

#extract main director
d1=strsplit(train$directors, split = ", ")
DF_2 <- structure(d1,row.names = c(NA,-1), class = "data.frame")
DF_transpose_2<-transpose(DF_2)
tags_d <- c('main director')
colnames(DF_transpose_2)<-tags_d
directors=DF_transpose_2[1]
train_clean=cbind(train_clean,directors)

#extract main writer
w1=strsplit(train$writers, split = ", ")
DF_3 <- structure(w1,row.names = c(NA,-1), class = "data.frame")
DF_transpose_3<-transpose(DF_3)
tags_w <- c('main writer')
colnames(DF_transpose_3)<-tags_w
writers=DF_transpose_3[1]
train_clean=cbind(train_clean,writers)

#delete original variables that have been modified as new variables
train_clean=select(train_clean,-cast,-directors,-writers)

#Add the length of movie info, movie title and consensus as 3 new variables
train_clean <- train_clean %>% 
  mutate(title_length=str_count(string=train_clean$movie_title,pattern='\\S+'),
         info_length=str_count(string=train_clean$movie_info,pattern='\\S+'),
         critics_consensus_length=str_count(string=train_clean$critics_consensus,pattern='\\S+'))

#transfer rating to dummy variable
train %>%
  group_by(rating) %>%
  summarise(total = n()) %>%
  arrange(desc(total))
train_clean$rating <- str_replace_all(train_clean$rating, "[^[:alnum:]]", "_")
rating_dummy <- dummy(train_clean$rating, sep = "_")
train_clean <- cbind(train_clean,rating_dummy)

#transfer genre to dummy variable
train_clean %>%
  group_by(genre) %>%
  summarise(total = n()) %>%
  arrange(desc(total))
t <- strsplit(train_clean$genre, split = ", ")
tags <- unique(str_trim(unlist(t)))
df2 <- as.data.frame(Reduce(cbind, lapply(tags, function(i) sapply(t, function(j) +(any(grepl(i, j), na.rm = TRUE))))))

# assign the tags as column names and clean original varibales that have been modified as new ones
names(df2) <- tags
train_clean <- cbind(train_clean,df2)
train_clean=select(train_clean,-movie_title,-rating,-genre,-tomatometer_status,-audience_status)

#clean cristics concensus
corpus = Corpus(VectorSource(train_clean$critics_consensus))
corpus = tm_map(corpus,FUN = content_transformer(tolower))
corpus = tm_map(corpus,FUN = removePunctuation)
corpus = tm_map(corpus,FUN = removeWords,c(stopwords('english')))
corpus = tm_map(corpus,FUN = stripWhitespace)

#create a dictionary
dict = findFreqTerms(DocumentTermMatrix(Corpus(VectorSource(train_clean$critics_consensus))),
                     lowfreq = 0)
dict_corpus = Corpus(VectorSource(dict))

#Stem document
corpus = tm_map(corpus,FUN = stemDocument)

#Create a document term matrix (tokenize)
dtm = DocumentTermMatrix(corpus)

#Remove Sparse Terms
xdtm = removeSparseTerms(dtm,sparse = 0.95)

#Complete Stems
xdtm = as.data.frame(as.matrix(xdtm))
colnames(xdtm) = stemCompletion(x = colnames(xdtm),
                                dictionary = dict_corpus,
                                type='prevalent')
colnames(xdtm) = make.names(colnames(xdtm))

#Browse tokens
sort(colSums(xdtm),decreasing = T)

#Document Term Matrix - tfidf
dtm_tfidf = DocumentTermMatrix(x=corpus,
                               control = list(weighting=function(x) weightTfIdf(x,normalize=F)))
xdtm_tfidf = removeSparseTerms(dtm_tfidf,sparse = 0.95)
xdtm_tfidf = as.data.frame(as.matrix(xdtm_tfidf))
colnames(xdtm_tfidf) = stemCompletion(x = colnames(xdtm_tfidf),
                                      dictionary = dict_corpus,
                                      type='prevalent')
colnames(xdtm_tfidf) = make.names(colnames(xdtm_tfidf))
sort(colSums(xdtm_tfidf),decreasing = T)

#add unique serial number to each observation
mid=1:length(train_clean$movie_info)
train_clean=cbind(mid,train_clean)

#get sentiment scores with jokers lexicon
library(lexicon)
scores=train_clean%>%
  select(mid,critics_consensus)%>%
  group_by(mid)%>%
  unnest_tokens(output = word, input=critics_consensus)%>%
  left_join(key_sentiment_jockers)%>%
  mutate_if(is.numeric,coalesce,0)%>%
  summarise(reviewSentiment = mean(value))

#round sentiment scores and add to the dataset
critics_consensus_scores=scores$reviewSentiment
train_clean=cbind(train_clean,critics_consensus_scores)
train_clean<-select(train_clean,-critics_consensus)
train_clean$critics_consensus_sentiscores=round(critics_consensus_scores,4)
train_clean<-select(train_clean,-critics_consensus_scores)

#extract brand name of studio names
s1<-strsplit(tolower(train$studio_name),split = ' ')
DF_4 <- structure(s1,row.names = c(NA,-2), class = "data.frame")
DF_transpose_4<-transpose(DF_4)
tags_s <- c('acronym_studio1','acronym_studio2')
colnames(DF_transpose_4)<-tags_s
acronym_studio=DF_transpose_4[1:2]
train_clean=cbind(train_clean,acronym_studio)
train_clean<-select(train_clean,-acronym_studio2)

#judge whether is top studio
library(tidyverse)
topstudio<-train_clean$acronym_studio1
studio<-topstudio %>%
  map(~ 
        str_detect(.x,"sony|dreamworks|columbia|twentieth|tristar|disney|20th|fox|warner|newline|paramount|universal") %>%
        ifelse(1,0) %>%  # 1 means is the studio a top studio
        str_c(collapse = " ") 
  ) %>%
  unlist()

#transfer topstudio variable into factors
train_clean$topstudio <- as.factor(studio)
class(train_clean$topstudio)

#delete studio acronym
train_clean<-select(train_clean,-acronym_studio1)

#overview and output the clean dataset
glimpse(train_clean)
write.csv(train_clean,'rottern_tomato_clean dataset.csv',row.names = F)
