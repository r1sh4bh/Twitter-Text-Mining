#############################################################################################
#############################################################################################
####################################################################?########################
############################# T-MOBILE  #####################################################
#############################  TWEETS   #####################################################
###########################################?#################################################
#############################################################################################
#############################################################################################
##################?##########################################################################
 
# Downloading tweets from tweeter

if(!require("rtweet")) install.packages("rtweet"); library("rtweet")
if(!require("tidyverse")) install.packages("tidyverse"); library("tidyverse?)
if (!require("wordcloud")) {
  install.packages("wordcloud",repos="https://cran.rstudio.com/",
                  quiet=TRUE)
  require("wordcloud")
}
for (i in c('SnowballC','slam','tm','Matrix','tidytext','dplyr','hunspell','purrr'
            ,'textste?','ggplot2','treemapify','treemap')){
  if (!require(i, character.only=TRUE)) install.packages(i, repos = "http://cran.us.r-project.org")
  require(i, character.only=TRUE)
}

# Authentication

app = 'fceli'
consumer_key = 'CONFIDENTIAL'
consumer_secret = '?ONFIDENTIAL'
access_token = 'CONFIDENTIAL'
access_secret = 'CONFIDENTIAL'

# Creation twitter_token

twitter_token <- create_token(
  app = app,
  consumer_key = consumer_key,
  consumer_secret = consumer_secret,
  access_token = access_token,
  access_sec?et = access_secret,
  set_renv=FALSE)


#Searching tweets by users from the company T-Mobile

tm_mobile_company=get_timeline(user=c('TMobileTruckPHM','TMobile','T_Labs','TMobileHelp','JohnLegere','SievertMike',
                                      'MetroB?TMobile','TMobileArena','TMobilePark','tmobilecareers','TMobileBusiness',
                                      'TMobileIR','TMobileNews'), n = 5000, include_rts=FALSE)

#Keeping only tweets in english

tm_mobile_company=tm_mobile_company[(tm_mobile_compan?$lang)=='en',]
save(tm_mobile_company, file = "C:/Users/azambranollano/OneDrive - IESEG/Alejandra/Social Media Analytics/Group Project/userscompany.RData")

# Loading data previously downloaded from T-Mobile accounts

load('C:/Users/fceli/Desktop/BIG DATA/?emester 2/1. Social media analytics/SMA-master/Group assignment/Ale files/handle.RData')
load('C:/Users/fceli/Desktop/BIG DATA/Semester 2/1. Social media analytics/SMA-master/Group assignment/Ale files/fcTmob3.RData')
load('C:/Users/fceli/Desktop/BIG DATA/?emester 2/1. Social media analytics/SMA-master/Group assignment/Ale files/rishtmobfullarch.RData')
load("C:/Users/fceli/Desktop/BIG DATA/Semester 2/1. Social media analytics/SMA-master/Group assignment/Ale files/userscompany.RData")

# Joinig final base fr?m users of the company

tmobile_company=rbind.data.frame(handle,fcTmob3,rishtmobfullarch,tm_mobile_company)

# Keeping only tweets in englisg, no retweets and deleting duplicate tweets

tmobile_company=tmobile_company[(tmobile_company$lang)=='en',]
tmobile?company=tmobile_company[!duplicated(tmobile_company$status_id),]
tmobile_company=tmobile_company[!(tmobile_company$is_retweet),]

# Saving the file for later use

save(tmobile_company, file = "C:/Users/fceli/Desktop/BIG DATA/Semester 2/1. Social media anal?tics/SMA-master/Group assignment/tmobile_company.RData")

# Searching tweets by keywords

tm_mobile3<-search_30day(q='#AreYouWithUs', n=5000,env_name = 'tweets30')
tm_mobile4<-search_30day(q='#TmobileTruck', n=5000,env_name = 'tweets30')
tm_mobile5<-search?30day(q='#BeMagenta', n=5000,env_name = 'tweets30')
tm_mobile6<-search_30day(q='#TMobile', n=5000,env_name = 'tweets30')
tm_mobile7<-search_30day(q='#TMobileBusiness', n=5000,env_name = 'tweets30')
tm_mobile8<-search_30day(q='#TMUS', n=5000,env_name = 'twe?ts30')
tm_mobile9<-search_30day(q='#TMobileCareers', n=5000,env_name = 'tweets30')
tm_mobile10<-search_30day(q='#IamMagenta', n=5000,env_name = 'tweets30')
tm_mobile11<-search_30day(q='#TeamMagenta', n=5000,env_name = 'tweets30')
tm_mobile12<-search_30day(?='#TeamMetro', n=5000,env_name = 'tweets30')
tm_mobile13<-search_30day(q='#5GThatWorks', n=5000,env_name = 'tweets30')

a=rbind.data.frame(tm_mobile3,tm_mobile4,tm_mobile5,tm_mobile6,tm_mobile7,tm_mobile8,
                   tm_mobile9,tm_mobile10,tm_mobil?11,tm_mobile12,tm_mobile13)

# Saving the file for later use

save(a, file = "C:/users/azambranollano/OneDrive - IESEG/Alejandra/Social Media Analytics/Group Project/actualizada.RData")

# Loading data previously downloaded for combined data sets

load('C:?Users/fceli/Desktop/BIG DATA/Semester 2/1. Social media analytics/SMA-master/Group assignment/Ale files/actualizada.RData')
load('C:/Users/fceli/Desktop/BIG DATA/Semester 2/1. Social media analytics/SMA-master/Group assignment/Ale files/tm24.RData')
load('?:/Users/fceli/Desktop/BIG DATA/Semester 2/1. Social media analytics/SMA-master/Group assignment/Ale files/tdayshashtags2.RData')

tmobile_users=rbind.data.frame(a,tm_mobile24,rishtmobfullarch,rish)
tmobile_users=tmobile_users[(tmobile_users$lang)=='en',]
t?obile_users=tmobile_users[!duplicated(tmobile_users$status_id),]
tmobile_users=tmobile_users[!(tmobile_users$is_retweet),]

#Keeping tweets differents from users of the company

tmobile_users = anti_join(tmobile_users, tmobile_company, by="status_id") 
tmo?ile_users = tmobile_users %>% 
  filter(!screen_name %in% c('TMobileTruckPHM','TMobile','T_Labs','TMobileHelp','JohnLegere','SievertMike',
                           'MetroByTMobile','TMobileArena','TMobilePark','tmobilecareers','TMobileBusiness',
        ?                  'TMobileIR','TMobileNews'))

save(tmobile_users, file = "C:/Users/fceli/Desktop/BIG DATA/Semester 2/1. Social media analytics/SMA-master/Group assignment/tmobile_users.RData")


################################# TOPIC AND SENTIMENT ANALYS?S  #################################*

for (i in c("tidytext", "SnowballC", "dplyr","tidyverse", "hunspell","topicmodels", "textstem", "ggplot2", "ldatuning", "utf8", "wordcloud2", "wordcloud", "RColorBrewer","flexdashboard")){  if (!require(i, character.o?ly=TRUE)) install.packages(i, repos = "http://cran.us.r-project.org")
  require(i, character.only=TRUE)
}

##### loading files
#Tweets generated from the company are stored as "tmobile_company.RData"

load('C:/Users/fceli/Desktop/BIG DATA/Semester 2/1. Soc?al media analytics/SMA-master/Group assignment/tmobile_company.RData')

#Tweets generated from different users  are stored as "tmobile_users.RData"

load("C:/Users/fceli/Desktop/BIG DATA/Semester 2/1. Social media analytics/SMA-master/Group assignment/tmob?le_users.RData")

#########################################
#########################################
###############            ##############
###############  CLEANING  ##############
###############            ##############
############################?############
#########################################

#subsetting the data

tm_comments <- tmobile_company[,2:6]

hash_comments <- tmobile_users[(tmobile_users$is_retweet==FALSE), c("status_id", "created_at", "screen_name", "source", "reply_to_screen_nam?", "text", "followers_count")]

#removing punctuations and numbers

tm_comments <- mutate(tm_comments, text = gsub(x = text, pattern = "[0-9]+|[[:punct:]]|\\(.*\\)|[']|[']", replacement = ""))
hash_comments <- mutate(hash_comments, text = gsub(x = text, pa?tern = "[0-9]+|[[:punct:]]|\\(.*\\)|[']|[']", replacement = ""))

hash_comments <- hash_comments %>%
  mutate(source2 = ifelse(source == "Twitter for Android", "Android", 
                          ifelse(source == "Twitter for iPhone", "iPhone", 
        ?                 ifelse(source == "Twitter Web App", "WebApp", "Other"))))

#tokenize, remove stop words and lemmatize

#add more custom stop words

custom_stop_words <- tribble(
  ~word,  ~lexicon,
  # Add custom stop words
  "tmobile", "CUSTOM",
  "johnl?gere",  "CUSTOM",
  "magenta", "CUSTOM",
  "telekomwall", "CUSTOM",
  "its", "CUSTOM",
  "were", "CUSTOM",
  "tmobilecareers", "CUSTOM",
  "areyouwithus", "CUSTOM",
  "bemagenta", "CUSTOM",
  "amp", "CUSTOM",
  "closerthanever", "CUSTOM",
  "youre", "CUSTO?",
  "tmobiletuesdys", "CUSTOM",
  "hey", "CUSTOM",
  "rondarousey", "CUSTOM",
  "sideshowjohn", "CUSTOM",
  "metropcs", "CUSTOM",
  "goldenknights", "CUSTOM",
  "tmobiletuesdays", "CUSTOM",
  "atomtickets", "CUSTOM",
  "goldenknights", "CUSTOM"
)

stop_wo?ds2 <- stop_words %>% 
  bind_rows(custom_stop_words)

tm_lemm <- tm_comments %>% 
  unnest_tokens(output = "word",
                input = text,
                token = "words",
                drop = FALSE, to_lower = TRUE) %>%
  anti_join(stop_words2)%>?
  mutate(word = lemmatize_words(word))

hash_lemm <- hash_comments %>% 
  unnest_tokens(output = "word",
                input = text,
                token = "words",
                drop = FALSE, to_lower = TRUE) %>%
  anti_join(stop_words2)%>%
  mutate?word = lemmatize_words(word))

#########################################
#########################################
###############    TOPIC  ###############
###############  MODELING ###############
#########################################
###############?#########################

#create the DTM

tm_DTM <- tm_lemm %>%
  count(screen_name, word, sort = TRUE) %>%
  cast_dtm(document = screen_name, term = word, value = n, weighting = tm::weightTf)

hash_DTM_reply <- hash_lemm %>%
  count(source2, word, sort ? TRUE) %>%
  cast_dtm(document = source2, term = word, value = n, weighting = tm::weightTf)

#find the optimal number of topics using LDA tuning

result <- FindTopicsNumber(
  tm_DTM,
  topics = seq(from = 2, to = 15, by = 1),
  metrics = c("Griffiths2004"? "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 77),
  mc.cores = 4L,
  verbose = TRUE
)
FindTopicsNumber_plot(result)

hash_result <- FindTopicsNumber(
  hash_DTM_reply,
  topics = seq(from = 2, to = 15, by = 1),
  ?etrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 77),
  mc.cores = 4L,
  verbose = TRUE
)

FindTopicsNumber_plot(hash_result)


tweets_lda <- LDA(tm_DTM, k = 3,method="gibbs",control = list(?eed = 77))
tweets_lda

save(tweets_lda, file = "C:/Users/fceli/Desktop/BIG DATA/Semester 2/1. Social media analytics/SMA-master/Group assignment/tmob_LDA.RData")


hashreply_tweets_lda <- LDA(hash_DTM_reply, k = 4,method="gibbs",control = list(seed = 77))
?ashreply_tweets_lda

save(hashreply_tweets_lda, file = "C:/Users/fceli/Desktop/BIG DATA/Semester 2/1. Social media analytics/SMA-master/Group assignment/users_LDA.RData")


# isolate the topics stored in "beta"

tweet_topics <- tidy(tweets_lda, matrix = "b?ta")

hashreply_topics <- tidy(hashreply_tweets_lda, matrix = "beta")

# get the top terms per topic

top_tweet_terms <- tweet_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)
top_tweet_terms

top_hashreply_ter?s <- hashreply_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)
top_hashreply_terms

#plot the top terms per topic

top_tweet_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, ?eta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered()

t(topics(tweets_lda,4))

top_hashreply_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplo?(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered()

t(topics(hashreply_tweets_lda,4))


#Which company handles are the most active

days_since <- tmobi?e_company %>%
  mutate(diff = as.numeric((difftime(max(as.Date.POSIXct(created_at)), created_at))/(24*60))) %>%
  group_by(screen_name) %>%
  arrange(desc(diff)) %>%
  slice(1) %>%
  select(screen_name, diff)

tmobile_company %>%
  group_by(screen_name) %>?
  count() %>%
  left_join(days_since) %>%
  mutate(monthly_freq = round(n/diff,1))

#engagement statistics for 

tmobile_company %>%
  group_by(screen_name) %>%
  summarise(sum(favorite_count), sum(retweet_count))

#handling user tweets 

tmobile_company ?>%
  filter(!is.na(reply_to_status_id)) %>%
  group_by(screen_name) %>%
  count(sort = TRUE)


#########################################
#########################################
###########    SENTIMENT    #############
###########    ANALYSIS     #######?#####
#########################################
#########################################

#########   Sentiment of "tm_lemm" - tweets from the company  #########

TMCSentiment1 <- inner_join(tm_lemm,get_sentiments("bing"))
TMCSentiment2 <- inner_join(tm_l?mm,get_sentiments("afinn"))
TMCSentiment3 <- inner_join(tm_lemm,get_sentiments("loughran"))
TMCSentiment4 <- inner_join(tm_lemm,get_sentiments("nrc"))

# most positive/negative words

summarySentiment1 <- TMCSentiment1 %>%  count(word,sentiment,sort=TRUE) ?>%
  group_by(sentiment) %>%
  top_n(10) %>%  
  arrange(n) %>%
  as.data.frame(stringsAsFactors=FALSE)

ggplot(summarySentiment1, aes(x = sentiment, y = n,fill=factor(sentiment))) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Sentiment Counts in ?ing",
    x = "Counts",
    y = "Sentiment"
  )

summarySentiment2 <- TMCSentiment2 %>%  count(word,value,sort=TRUE) %>%
  group_by(value) %>%
  top_n(10) %>%  
  arrange(n) %>%
  as.data.frame(stringsAsFactors=FALSE)

ggplot(summarySentiment2, aes(x = val?e, y = n,fill=factor(value))) +
  geom_col() +
  coold_flip() +
  labs(
    title = "Sentiment Counts in afinn",
    x = "Counts",
    y = "Sentiment"
  )

summarySentiment3 <- TMCSentiment3 %>%  count(word,sentiment,sort=TRUE) %>%
  group_by(sentiment) %>?
  top_n(10) %>%  
  arrange(n) %>%
  as.data.frame(stringsAsFactors=FALSE)

ggplot(summarySentiment3, aes(x = sentiment, y = n,fill=factor(sentiment))) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Sentiment Counts in loughran",
    x = "Counts",?    y = "Sentiment"
  )

summarySentiment4 <- TMCSentiment4 %>%  count(word,sentiment,sort=TRUE) %>%
  group_by(sentiment) %>%
  top_n(10) %>%  
  arrange(n) %>%
  as.data.frame(stringsAsFactors=FALSE)

ggplot(summarySentiment4, aes(x = sentiment, y = n, f?ll=factor(sentiment))) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Sentiment Counts in NCR",
    x = "Counts",
    y = "Sentiment"
  )

# sentiment per tweet
TMCTokenized2 <- tm_lemm
sent_tweet<- TMCTokenized2 %>%
  inner_join(get_sentiments("af?nn")) %>%
  group_by(text, status_id) %>%
  summarize(sentiment = sum(value)) %>%
  arrange(sentiment)

tmobile_company2<-tmobile_company
tmobile_company2<- tmobile_company2 %>% left_join(sent_tweet, by='status_id') %>%
  arrange(sentiment)
colnames(tmobil?_company2)
sum(is.na(tmobile_company2$sentiment))
tmobile_company2 <-tmobile_company2%>%drop_na(sentiment)


save(tmobile_company2, file = "C:/Users/fceli/Desktop/BIG DATA/Semester 2/1. Social media analytics/SMA-master/Group assignment/tmobile_company3.Rd?ta")

# NCR - analysis per sentiment

as.data.frame(table(get_sentiments("nrc")$sentiment)) %>%
  arrange(desc(Freq))

trust <- get_sentiments("nrc") %>%
  filter(sentiment == "trust")
TMC_trust<-TMCTokenized2 %>%
  inner_join(trust) %>%
  count(word, sort?= TRUE)
TMC_trust2<-TMC_trust
TMC_trust2["sentiment" <- "trust"]


surprise <- get_sentiments("nrc") %>%
  filter(sentiment == "surprise")
TMC_surprise<-TMCTokenized2 %>%
  inner_join(surprise) %>%
  count(word, sort = TRUE)
TMC_surprise2<-TMC_surprise
TMC?surprise2["sentiment"]<- "surprise"



sadness <- get_sentiments("nrc") %>%
  filter(sentiment == "sadness")
TMC_sadness<-TMCTokenized2 %>%
  inner_join(sadness) %>%
  count(word, sort = TRUE)
TMC_sadness2<-TMC_sadness
TMC_sadness2["sentiment"]<- "sadness"?

positive <- get_sentiments("nrc") %>%
  filter(sentiment == "positive")
TMC_positive<-TMCTokenized2 %>%
  inner_join(positive) %>%
  count(word, sort = TRUE)
TMC_positive2<-TMC_positive
TMC_positive2["sentiment"]<- "positive"



negative <- get_sentiment?("nrc") %>%
  filter(sentiment == "negative")
TMC_negative<-TMCTokenized2 %>%
  inner_join(negative) %>%
  count(word, sort = TRUE)
TMC_negative2<-TMC_negative
TMC_negative2["sentiment"]<- "negative"



joy <- get_sentiments("nrc") %>%
  filter(sentiment =? "joy")
TMC_joy<-TMCTokenized2 %>%
  inner_join(joy) %>%
  count(word, sort = TRUE)
TMC_joy2<-TMC_joy
TMC_joy2["sentiment"]<- "joy"



fear <- get_sentiments("nrc") %>%
  filter(sentiment == "fear")
TMC_fear<-TMCTokenized2 %>%
  inner_join(fear) %>%
  coun?(word, sort = TRUE)
TMC_fear2<-TMC_fear
TMC_fear2["sentiment"]<- "fear"



disgust <- get_sentiments("nrc") %>%
  filter(sentiment == "disgust")
TMC_disgust<-TMCTokenized2 %>%
  inner_join(disgust) %>%
  count(word, sort = TRUE)
TMC_disgust2<-TMC_disgust
T?C_disgust2["sentiment"]<- "disgust"



anticipation <- get_sentiments("nrc") %>%
  filter(sentiment == "anticipation")
TMC_anticipation<-TMCTokenized2 %>%
  inner_join(anticipation) %>%
  count(word, sort = TRUE)
TMC_anticipation2<-TMC_anticipation
TMC_ant?cipation2["sentiment"]<- "anticipation"



anger <- get_sentiments("nrc") %>%
  filter(sentiment == "anger")
TMC_anger<-TMCTokenized2 %>%
  inner_join(anger) %>%
  count(word, sort = TRUE)
TMC_anger2<-TMC_anger
TMC_anger2["sentiment"]<- "anger"




company?sentiment <- rbind(TMC_trust2,TMC_surprise2,TMC_sadness2,
                           TMC_positive2,TMC_negative2,TMC_joy2,TMC_fear2,
                           TMC_disgust2,TMC_anticipation2,TMC_anger2)


company_sentiment["source"] <- "T-Mobile Tweets"


? cloud per sentiment NCR

set.seed(1234)
wordcloud(TMC-anger$word,TMC_anger$n,min.freq = 1, colors=brewer.pal(8, "Dark2"),random.order=FALSE, rot.per=0.35, shape = 'star', max.words=300)
wordcloud2(TMC_anticipation, color = "random-light", backgroundColor ? "white",size = 1, shape = 'cardioid')
wordcloud2(TMC_disgust, color = "random-light", backgroundColor = "white",size = 1, shape = 'diamond')
wordcloud2(TMC_fear, color = "random-light", backgroundColor = "white",size = 1, shape = 'triangle-forward')
wordc?oud2(TMC_joy, color = "random-light", backgroundColor = "white",size = 1, shape = 'triangle')
wordcloud2(TMC_negative, color = "random-light", backgroundColor = "white",size = 1, shape = 'pentagon')
wordcloud2(TMC_positive, color = "random-light", backgrou?dColor = "white",size = 2, shape = 'star')
wordcloud2(TMC_surprise, color = "random-light", backgroundColor = "white",size = 2, shape = 'circle')
wordcloud2(TMC_trust, color = "random-light", backgroundColor = "white",size = 1, shape = 'star')

tmobile_com?any3<-tmobile_company2
#label a tweet with the hour
tmobile_company3$hour=sapply(tmobile_company3$created_at, function(x) {p=as.POSIXlt(x);p$hour})
#label a tweet with a number corresponding to the day of the week
tmobile_company3$wday=sapply(tmobile_compa?y3$created_at, function(x) {p=as.POSIXlt(x);p$wday})


tmobile_company3 %>%
  mutate(sent=ifelse(sentiment>0, "+ve", "-ve")) %>%
  ggplot(aes(x=wday,y=hour,fill=factor(sent), color=sent)) + 
  geom_jitter()





#########   Sentiment of "hash_lemm" - tweet? from different users  #########

#Sentiment

hashTokenized1 <- inner_join(hash_lemm,get_sentiments("bing"))
hashTokenized2 <- inner_join(hash_lemm,get_sentiments("afinn"))
hashTokenized3 <- inner_join(hash_lemm,get_sentiments("loughran"))
hashTokenized4 <? inner_join(hash_lemm,get_sentiments("nrc"))

# most positive/negative words

hashsummSent1 <- hashTokenized1 %>%  count(word,sentiment,sort=TRUE) %>%
  group_by(sentiment) %>%
  top_n(10) %>%  
  arrange(n) %>%
  as.data.frame(stringsAsFactors=FALSE)

ggp?ot(hashsummSent1, aes(x = sentiment, y = n, fill=factor(sentiment))) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Sentiment Counts in bing",
    x = "Counts",
    y = "Sentiment"
  )

hashsummSent2 <- hashTokenized2 %>%  count(word,value,sort=TRU?) %>%
  group_by(value) %>%
  top_n(10) %>%  
  arrange(n) %>%
  as.data.frame(stringsAsFactors=FALSE)

ggplot(hashsummSent2, aes(x = value, y = n, fill=factor(value))) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Sentiment Counts in afinn",
    ? = "Counts",
    y = "Sentiment"
  )

hashsummSent3 <- hashTokenized3 %>%  count(word,sentiment,sort=TRUE) %>%
  group_by(sentiment) %>%
  top_n(10) %>%  
  arrange(n) %>%
  as.data.frame(stringsAsFactors=FALSE)

ggplot(hashsummSent3, aes(x = sentiment, y ? n, fill=factor(sentiment))) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Sentiment Counts in loughran",
    x = "Counts",
    y = "Sentiment"
  )

hashsummSent4 <- hashTokenized4 %>%  count(word,sentiment,sort=TRUE) %>%
  group_by(sentiment) %>%?  top_n(10) %>%  
  arrange(n) %>%
  as.data.frame(stringsAsFactors=FALSE)

ggplot(hashsummSent4, aes(x = sentiment, y = n, fill=factor(sentiment))) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Sentiment Counts in ncr",
    x = "Counts",
    y = ?Sentiment"
  )

# sentiment per tweet

TMCTokenized3 <- hash_lemm
sent_tweet2<- TMCTokenized3 %>%
  inner_join(get_sentiments("afinn")) %>%
  group_by(text, status_id) %>%
  summarise(sentiment = sum(value)) %>%
  arrange(sentiment)

tmobile_users2<-tmobil?_users
tmobile_users2<- tmobile_users2 %>% left_join(sent_tweet2, by='status_id') %>%
  arrange(sentiment)
colnames(tmobile_users2)
sum(is.na(tmobile_users2$sentiment))
tmobile_users2<-tmobile_users2%>%drop_na(sentiment)

save(tmobile_users2, file = "C:/Us?rs/fceli/Desktop/BIG DATA/Semester 2/1. Social media analytics/SMA-master/Group assignment/tmobile_users2.Rdata")

# NCR - analysis per sentiment

as.data.frame(table(get_sentiments("nrc")$sentiment)) %>%
  arrange(desc(Freq))

trusthash <- get_sentiments(?nrc") %>%
  filter(sentiment == "trust")
hash_trust<-hashTokenized2 %>%
  inner_join(trust) %>%
  count(word, sort = TRUE)
hash_trust2<-hash_trust
hash_trust2["sentiment"]<- "trust"



surprisehash <- get_sentiments("nrc") %>%
  filter(sentiment == "surpri?e")
hash_surprise<-hashTokenized2 %>%
  inner_join(surprise) %>%
  count(word, sort = TRUE)
hash_surprise2<-hash_surprise
hash_surprise2["sentiment"]<- "surprise"



sadnesshash <- get_sentiments("nrc") %>%
  filter(sentiment == "sadness")
hash_sadness<-ha?hTokenized2 %>%
  inner_join(sadness) %>%
  count(word, sort = TRUE)
hash_sadness2<-hash_sadness
hash_sadness2["sentiment"]<- "sadness"



positivehash <- get_sentiments("nrc") %>%
  filter(sentiment == "positive")
hash_positive<-hashTokenized2 %>%
  inner?join(positive) %>%
  count(word, sort = TRUE)
hash_positive2<-hash_positive
hash_positive2["sentiment"]<- "positive"



negativehash <- get_sentiments("nrc") %>%
  filter(sentiment == "negative")
hash_negative<-hashTokenized2 %>%
  inner_join(negative) %>%?  count(word, sort = TRUE)
hash_negative2<-hash_negative
hash_negative2["sentiment"]<- "negative"



joyhash <- get_sentiments("nrc") %>%
  filter(sentiment == "joy")
hash_joy<-hashTokenized2 %>%
  inner_join(joy) %>%
  count(word, sort = TRUE)
hash_joy2<-?ash_joy
hash_joy2["sentiment"]<- "joy"



fearhash <- get_sentiments("nrc") %>%
  filter(sentiment == "fear")
hash_fear<-hashTokenized2 %>%
  inner_join(fear) %>%
  count(word, sort = TRUE)
hash_fear2<-hash_fear
hash_fear2["sentiment"]<- "fear"



disgusth?sh <- get_sentiments("nrc") %>%
  filter(sentiment == "disgust")
hash_disgust<-hashTokenized2 %>%
  inner_join(disgust) %>%
  count(word, sort = TRUE)
hash_disgust2<-hash_disgust
hash_disgust2["sentiment"]<- "disgust"



anticipationhash <- get_sentiments(?nrc") %>%
  filter(sentiment == "anticipation")
hash_anticipation<-hashTokenized2 %>%
  inner_join(anticipation) %>%
  count(word, sort = TRUE)
hash_anticipation2<-hash_anticipation
hash_anticipation2["sentiment"]<- "anticipation"


angerhash <- get_sentim?nts("nrc") %>%
  filter(sentiment == "anger")
hash_anger<-hashTokenized2 %>%
  inner_join(anger) %>%
  count(word, sort = TRUE)
hash_anger2<-hash_anger
hash_anger2["sentiment"]<- "anger"


user_sentiment <- rbind(hash_trust2,hash_surprise2,hash_sadness2,
 ?                      hash_positive2,hash_negative2,hash_joy2,hash_fear2,
                        hash_disgust2,hash_anticipation2,hash_anger2)
user_sentiment["source"] <- "Users Tweets"

# combining the sentiment file 'NCR' in 1 file

total_sentiment <- r?ind(user_sentiment, company_sentiment)

save(total_sentiment, file = "C:/Users/fceli/Desktop/BIG DATA/Semester 2/1. Social media analytics/SMA-master/Group assignment/total_sentiment.Rdata")


# cloud per sentiment NCR

set.seed(1234)
wordcloud(hash_anger$?ord,hash_anger$n,min.freq = 1, colors=brewer.pal(8, "Dark2"),random.order=FALSE, rot.per=0.35, shape = 'star', max.words=300)
wordcloud2(hash_anticipation, color = "random-light", backgroundColor = "white",size = 1, shape = 'cardioid')
wordcloud2(hash_disg?st, color = "random-light", backgroundColor = "white",size = 1, shape = 'diamond')
wordcloud2(hash_fear, color = "random-light", backgroundColor = "white",size = 1, shape = 'triangle-forward')
wordcloud2(hash_joy, color = "random-light", backgroundColor = ?white",size = 1, shape = 'triangle')
wordcloud2(hash_negative, color = "random-light", backgroundColor = "white",size = 1, shape = 'pentagon')
wordcloud2(hash_positive, color = "random-light", backgroundColor = "white",size = 2, shape = 'star')
wordcloud2(?ash_surprise, color = "random-light", backgroundColor = "white",size = 2, shape = 'circle')
wordcloud2(hash_trust, color = "random-light", backgroundColor = "white",size = 1, shape = 'star')



#label a tweet with the hour
tmobile_users2$hour=sapply(tmobil?_users2$created_at, function(x) {p=as.POSIXlt(x);p$hour})
#label a tweet with a number corresponding to the day of the week
tmobile_users2$wday=sapply(tmobile_users2$created_at, function(x) {p=as.POSIXlt(x);p$wday})

mypalette<-brewer.pal(2,"Greens")

tmob?le_users2 %>%
  mutate(sent=ifelse(sentiment>0, "+ve", "-ve")) %>%
  ggplot(aes(x=wday,y=hour,fill=factor(sent), color=sent)) + 
  geom_jitter()
