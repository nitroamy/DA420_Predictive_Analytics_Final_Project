# https://towardsdatascience.com/twitter-sentiment-analysis-and-visualization-using-r-22e1f70f6967

library(dplyr)
library(tidyr)
library(tidytext)
library(ggplot2)
library(purrr)
library(tidyverse)
library(tibble)
library(twitteR)
library(ROAuth)
library(wordcloud)
library(reshape2)
library(RColorBrewer)

# Loading credentials
consumer_key <- 'my customer key'
consumer_secret <- 'my secret key'
access_token <- 'my token'
access_secret <- 'my access key'
# Setting up to authenticate
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

tweets_bernie <- searchTwitter("#bernie", n=1000,lang = "en")
tweets_biden <- searchTwitter("#biden", n=1000,lang = "en")
tweets_trump <- searchTwitter("#trump", n=1000,lang = "en")
tweets_election2020 <- searchTwitter("#election2020", n=2000,lang = "en")

# Striping retweets
no_rt_bernie <- strip_retweets(tweets_bernie)
no_rt_biden <- strip_retweets(tweets_biden)
no_rt_trump <- strip_retweets(tweets_trump)
no_rt_election2020 <- strip_retweets(tweets_election2020)

# Converting extracted tweets without retweet to dataframe
bernie <- twListToDF(no_rt_bernie)
biden <-twListToDF(no_rt_biden)
trump <- twListToDF(no_rt_trump)
election2020 <- twListToDF(no_rt_election2020)

#bernie tweets
#remove unnecessary elements include: link, username, emoji, numbers
tweets.bernie = bernie %>% select(screenName, text)
tweets.bernie$clean_text <- gsub("http\\S+", " ", tweets.bernie$text)
tweets.bernie$clean_text <- gsub("@\\w+", " ", tweets.bernie$clean_text)
tweets.bernie$clean_text <- gsub("[^\x01-\x7F]", " ", tweets.bernie$clean_text)
tweets.bernie$clean_text <- gsub("[[:digit:]]", " ", tweets.bernie$clean_text)
# Removing "trump" since trump is considered as positive polarity and emotion during text analysis
tweets.bernie$clean_text <- gsub("Trump", " ", tweets.bernie$clean_text)
tweets.bernie$clean_text <- gsub("trump", " ", tweets.bernie$clean_text)
tweets.bernie$clean_text <- gsub("TRUMP", " ", tweets.bernie$clean_text)
#unnext_tokens() function to convert to lowercase, remove punctuation
tweets.bernie_stem <- tweets.bernie %>%
  select(clean_text) %>%
  unnest_tokens(word, clean_text)
#remove stop words
cleaned_tweets.bernie <- tweets.bernie_stem %>%
  anti_join(stop_words)
#bing sentiment analysis
bing_bernie = cleaned_tweets.bernie %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=TRUE) %>%
  ungroup()
bing_bernie
#plot top 10 negative and positive
bing_bernie %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word,n)) %>%
  ggplot(aes(word,n,fill=sentiment))+
  geom_col(show.legend=FALSE)+
  facet_wrap(~sentiment,scale="free_y")+
  labs(title="Tweets contatining '#bernie'", y="Contribution to sentiment", x=NULL)+
  coord_flip()+theme_bw()
#Polarity plot
polar_bar_bernie <- bing_bernie %>%
  group_by(sentiment) %>%
  summarise(word_count = n()) %>%
  ungroup() %>%
  mutate(sentiment = reorder(sentiment, word_count)) %>%
  #Use `fill = -word_count` to make the larger bars darker
  ggplot(aes(sentiment, word_count, fill = c("blue","red"))) +
  geom_col() +
  guides(fill = FALSE) + #Turn off the legend
  labs(x = NULL, y = "Word Count") +
  ggtitle("Bernie Tweets Polarity") +
  coord_flip()
polar_bar_bernie


#biden tweets
#remove unnecessary elements include: link, username, emoji, numbers
tweets.biden = biden %>% select(screenName, text)
tweets.biden$clean_text <- gsub("http\\S+", " ", tweets.biden$text)
tweets.biden$clean_text <- gsub("@\\w+", " ", tweets.biden$clean_text)
tweets.biden$clean_text <- gsub("[^\x01-\x7F]", " ", tweets.biden$clean_text)
tweets.biden$clean_text <- gsub("[[:digit:]]", " ", tweets.biden$clean_text)
# Removing "trump" since trump is considered as positive polarity and emotion during text analysis
tweets.biden$clean_text <- gsub("Trump", " ", tweets.biden$clean_text)
tweets.biden$clean_text <- gsub("trump", " ", tweets.biden$clean_text)
tweets.biden$clean_text <- gsub("TRUMP", " ", tweets.biden$clean_text)
#unnext_tokens() function to convert to lowercase, remove punctuation
tweets.biden_stem <- tweets.biden %>%
  select(clean_text) %>%
  unnest_tokens(word, clean_text)
#remove stop words
cleaned_tweets.biden <- tweets.biden_stem %>%
  anti_join(stop_words)
#bing sentiment analysis
bing_biden = cleaned_tweets.biden %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=TRUE) %>%
  ungroup()
bing_biden
#plot top 10 negative and positive
bing_biden %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word,n)) %>%
  ggplot(aes(word,n,fill=sentiment))+
  geom_col(show.legend=FALSE)+
  facet_wrap(~sentiment,scale="free_y")+
  labs(title="Tweets contatining '#biden'", y="Contribution to sentiment", x=NULL)+
  coord_flip()+theme_bw()
#Polarity plot
polar_bar_biden <- bing_biden %>%
  group_by(sentiment) %>%
  summarise(word_count = n()) %>%
  ungroup() %>%
  mutate(sentiment = reorder(sentiment, word_count)) %>%
  #Use `fill = -word_count` to make the larger bars darker
  ggplot(aes(sentiment, word_count, fill = c("blue","red"))) +
  geom_col() +
  guides(fill = FALSE) + #Turn off the legend
  labs(x = NULL, y = "Word Count") +
  ggtitle("Biden Tweets Polarity") +
  coord_flip()
polar_bar_biden

#trump tweets
#remove unnecessary elements include: link, username, emoji, numbers
tweets.trump = trump %>% select(screenName, text)
tweets.trump$clean_text <- gsub("http\\S+", " ", tweets.trump$text)
tweets.trump$clean_text <- gsub("@\\w+", " ", tweets.trump$clean_text)
tweets.trump$clean_text <- gsub("[^\x01-\x7F]", " ", tweets.trump$clean_text)
tweets.trump$clean_text <- gsub("[[:digit:]]", " ", tweets.trump$clean_text)
# Removing "trump" since trump is considered as positive polarity and emotion during text analysis
tweets.trump$clean_text <- gsub("Trump", " ", tweets.trump$clean_text)
tweets.trump$clean_text <- gsub("trump", " ", tweets.trump$clean_text)
tweets.trump$clean_text <- gsub("TRUMP", " ", tweets.trump$clean_text)
#unnext_tokens() function to convert to lowercase, remove punctuation
tweets.trump_stem <- tweets.trump %>%
  select(clean_text) %>%
  unnest_tokens(word, clean_text)
#remove stop words
cleaned_tweets.trump <- tweets.trump_stem %>%
  anti_join(stop_words)
#bing sentiment analysis
bing_trump = cleaned_tweets.trump %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=TRUE) %>%
  ungroup()
bing_trump
#plot top 10 negative and positive
bing_trump %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word,n)) %>%
  ggplot(aes(word,n,fill=sentiment))+
  geom_col(show.legend=FALSE)+
  facet_wrap(~sentiment,scale="free_y")+
  labs(title="Tweets contatining '#trump'", y="Contribution to sentiment", x=NULL)+
  coord_flip()+theme_bw()
#Polarity plot
polar_bar_trump <- bing_trump %>%
  group_by(sentiment) %>%
  summarise(word_count = n()) %>%
  ungroup() %>%
  mutate(sentiment = reorder(sentiment, word_count)) %>%
  #Use `fill = -word_count` to make the larger bars darker
  ggplot(aes(sentiment, word_count, fill = c("blue","red"))) +
  geom_col() +
  guides(fill = FALSE) + #Turn off the legend
  labs(x = NULL, y = "Word Count") +
  ggtitle("Trump Tweets Polarity") +
  coord_flip()
polar_bar_trump

#election2020 tweets
#remove unnecessary elements include: link, username, emoji, numbers
tweets.election = election2020 %>% select(screenName, text)
tweets.election$clean_text <- gsub("http\\S+", " ", tweets.election$text)
tweets.election$clean_text <- gsub("@\\w+", " ", tweets.election$clean_text)
tweets.election$clean_text <- gsub("[^\x01-\x7F]", " ", tweets.election$clean_text)
tweets.election$clean_text <- gsub("[[:digit:]]", " ", tweets.election$clean_text)
# Removing "trump" since trump is considered as positive polarity and emotion during text analysis
tweets.election$clean_text <- gsub("Trump", " ", tweets.election$clean_text)
tweets.election$clean_text <- gsub("trump", " ", tweets.election$clean_text)
tweets.election$clean_text <- gsub("TRUMP", " ", tweets.election$clean_text)
#unnext_tokens() function to convert to lowercase, remove punctuation
tweets.election_stem <- tweets.election %>%
  select(clean_text) %>%
  unnest_tokens(word, clean_text)
#remove stop words
cleaned_tweets.election <- tweets.election_stem %>%
  anti_join(stop_words)
#bing sentiment analysis
bing_election = cleaned_tweets.election %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=TRUE) %>%
  ungroup()
bing_election
#plot top 10 negative and positive
bing_election %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word,n)) %>%
  ggplot(aes(word,n,fill=sentiment))+
  geom_col(show.legend=FALSE)+
  facet_wrap(~sentiment,scale="free_y")+
  labs(title="Tweets contatining '#election2020'", y="Contribution to sentiment", x=NULL)+
  coord_flip()+theme_bw()
#Polarity plot
polar_bar_election <- bing_election %>%
  group_by(sentiment) %>%
  summarise(word_count = n()) %>%
  ungroup() %>%
  mutate(sentiment = reorder(sentiment, word_count)) %>%
  #Use `fill = -word_count` to make the larger bars darker
  ggplot(aes(sentiment, word_count, fill = c("blue","red"))) +
  geom_col() +
  guides(fill = FALSE) + #Turn off the legend
  labs(x = NULL, y = "Word Count") +
  ggtitle("Election2020 Tweets Polarity") +
  coord_flip()
polar_bar_election

#sentiment score
sentiment_bing = function(twt){
  twt_tbl = tibble(text=twt) %>%  #text cleaning, remove "trump" as well since it is considered as positive sentiment
    mutate(
      stripped_text=gsub("http\\S+"," ",text),
      stripped_text=gsub("TRUMP", " ",stripped_text), 
      stripped_text=gsub("Trump", " ",stripped_text),
      stripped_text=gsub("trump", " ",stripped_text)
    ) %>%
    unnest_tokens(word,stripped_text) %>%
    anti_join(stop_words) %>%
    inner_join(get_sentiments("bing")) %>%
    count(word,sentiment, sort= TRUE) %>%
    ungroup() %>%
    mutate(
      score= case_when( #create a column "score
        sentiment == 'negative'~n*(-1), #assigns -1 when negative word
        sentiment == 'positive'~n*1) #assings 1 when positive word
      )
  sent.score=case_when(
    nrow(twt_tbl)==0~0, #if there are no words, score is 0
    nrow(twt_tbl)>0~sum(twt_tbl$score) #otherwise, sum the positive and negatives
  )
  zero.type=case_when(
    nrow(twt_tbl)==0~"Type1", #no words at all, zero=no
    nrow(twt_tbl)>0~"Type2" #zero means sum of words=0
  )
  list(score= sent.score, type=zero.type, twt_tbl=twt_tbl)
}

#apply function: retuns a list of all the sentiment scores, types and tables of the tweets
bernie_sent = lapply(bernie$text, function(x){sentiment_bing(x)})
biden_sent = lapply(biden$text, function(x){sentiment_bing(x)})
trump_sent = lapply(trump$text, function(x){sentiment_bing(x)})
election_sent = lapply(election2020$text, function(x){sentiment_bing(x)})

#create a tibble specifying the #keywords, sentiment scores and types 
tweets_sentiment = bind_rows(
  tibble(
  keyword='#bernie',
  score=unlist(map(bernie_sent, 'score')),
  type=unlist(map(bernie_sent, 'type'))
),
tibble(
  keyword='#biden',
  score=unlist(map(biden_sent, 'score')),
  type=unlist(map(biden_sent, 'type'))
),
tibble(
  keyword='#trump',
  score=unlist(map(trump_sent, 'score')),
  type=unlist(map(trump_sent, 'type'))
)
)

election_sentiment= tibble(
  keyword='#election2020',
  score=unlist(map(election_sent, 'score')),
  type=unlist(map(election_sent, 'type'))
)


#plot histograms of tweets sentiment for three candidate
ggplot(tweets_sentiment,aes(x=score, fill=keyword)) + 
  geom_histogram(bins=10, alpha=0.6) + 
  facet_grid(~keyword) + theme_bw()
#plot histogram of tweets sentiment for election 2020
ggplot(election_sentiment,aes(x=score, fill=keyword)) + 
  geom_histogram(bins=10, alpha=0.6) + 
  theme_bw()

#https://www.tidytextmining.com/sentiment.html
#https://cran.r-project.org/web/packages/syuzhet/vignettes/syuzhet-vignette.html
#https://www.datacamp.com/community/tutorials/sentiment-analysis-R

# NRC emotion sentiment analysis
#bernie
nrc_bernie = cleaned_tweets.bernie %>%
  inner_join(get_sentiments("nrc")) %>%
  filter(!sentiment %in% c("positive","negative")) %>%
  count(word, sentiment, sort=TRUE) %>%
  ungroup()
nrc_bernie

#plot
bernie_plot <- nrc_bernie %>%
  group_by(sentiment) %>%
  summarise(word_count = n()) %>%
  ungroup() %>%
  mutate(sentiment = reorder(sentiment, word_count)) %>%
  #Use `fill = -word_count` to make the larger bars darker
  ggplot(aes(sentiment, word_count, fill = -word_count)) +
  geom_col() +
  guides(fill = FALSE) + #Turn off the legend
  labs(x = NULL, y = "Word Count") +
  ggtitle("Bernie NRC Sentiment") +
  coord_flip()
bernie_plot

#biden
nrc_biden = cleaned_tweets.biden %>%
  inner_join(get_sentiments("nrc")) %>%
  filter(!sentiment %in% c("positive","negative")) %>%
  count(word, sentiment, sort=TRUE) %>%
  ungroup()
nrc_biden
#plot
biden_plot <- nrc_biden %>%
  group_by(sentiment) %>%
  summarise(word_count = n()) %>%
  ungroup() %>%
  mutate(sentiment = reorder(sentiment, word_count)) %>%
  #Use `fill = -word_count` to make the larger bars darker
  ggplot(aes(sentiment, word_count, fill = -word_count)) +
  geom_col() +
  guides(fill = FALSE) + #Turn off the legend
  labs(x = NULL, y = "Word Count") +
  ggtitle("Biden NRC Sentiment") +
  coord_flip()
biden_plot

#trump
nrc_trump = cleaned_tweets.trump %>%
  inner_join(get_sentiments("nrc")) %>%
  filter(!sentiment %in% c("positive","negative")) %>%
  count(word, sentiment, sort=TRUE) %>%
  ungroup()
nrc_trump
#plot
trump_plot <- nrc_trump %>%
  group_by(sentiment) %>%
  summarise(word_count = n()) %>%
  ungroup() %>%
  mutate(sentiment = reorder(sentiment, word_count)) %>%
  #Use `fill = -word_count` to make the larger bars darker
  ggplot(aes(sentiment, word_count, fill = -word_count)) +
  geom_col() +
  guides(fill = FALSE) + #Turn off the legend
  labs(x = NULL, y = "Word Count") +
  ggtitle("Trump NRC Sentiment") +
  coord_flip()
trump_plot

#election2020
nrc_election = cleaned_tweets.election %>%
  inner_join(get_sentiments("nrc")) %>%
  filter(!sentiment %in% c("positive","negative")) %>%
  count(word, sentiment, sort=TRUE) %>%
  ungroup()
nrc_trump
#plot
election_plot <- nrc_election %>%
  group_by(sentiment) %>%
  summarise(word_count = n()) %>%
  ungroup() %>%
  mutate(sentiment = reorder(sentiment, word_count)) %>%
  #Use `fill = -word_count` to make the larger bars darker
  ggplot(aes(sentiment, word_count, fill = -word_count)) +
  geom_col() +
  guides(fill = FALSE) + #Turn off the legend
  labs(x = NULL, y = "Word Count") +
  ggtitle("Election2020 NRC Sentiment") +
  coord_flip()
election_plot


#wordcloud
#https://www.r-bloggers.com/thrice-sentiment-analysis-emotions-in-lyrics/
#common wordcloud
cleaned_tweets.bernie %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100, colors=brewer.pal(8, "Dark2")))

cleaned_tweets.biden %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100, colors=brewer.pal(8, "Dark2")))

cleaned_tweets.trump %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100, colors=brewer.pal(8, "Dark2")))

cleaned_tweets.election %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100, colors=brewer.pal(8, "Dark2")))

#polarity
cleaned_tweets.bernie %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("brown", "dark green"),
                   title.size=1, max.words = 50)

cleaned_tweets.biden %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("brown", "dark green"),
                   title.size=1, max.words = 50)

cleaned_tweets.trump %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("brown", "dark green"),
                   title.size=1, max.words = 50)

cleaned_tweets.election %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("brown", "dark green"),
                   title.size=1, max.words = 50)

#emotion
#https://tabvizexplorer.com/sentiment-analysis-using-r-and-twitter/
#https://rpubs.com/SulmanKhan/437587
cleaned_tweets.bernie %>%
  inner_join(get_sentiments("nrc")) %>%
  filter(!sentiment %in% c("positive","negative")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = brewer.pal(8, "Dark2"),
                   title.size=1, max.words=50, random.order = FALSE)

cleaned_tweets.biden %>%
  inner_join(get_sentiments("nrc")) %>%
  filter(!sentiment %in% c("positive","negative")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = brewer.pal(8, "Dark2"),
                   title.size=1, max.words=50, random.order = FALSE)

cleaned_tweets.trump %>%
  inner_join(get_sentiments("nrc")) %>%
  filter(!sentiment %in% c("positive","negative")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = brewer.pal(8, "Dark2"),
                   title.size=1, max.words=50, random.order = FALSE)

cleaned_tweets.election %>%
  inner_join(get_sentiments("nrc")) %>%
  filter(!sentiment %in% c("positive","negative")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = brewer.pal(8, "Dark2"),
                   title.size=1, max.words=50, random.order = FALSE)
