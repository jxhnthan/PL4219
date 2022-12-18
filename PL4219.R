# install packages
install.packages("RedditExtractoR")
install.packages("stringr", dependencies=TRUE)
install.packages("dplyr", dependencies=TRUE)
install.packages("ggplot2", dependencies=TRUE)
install.packages("tidytext", dependencies=TRUE)
install.packages("textdata", dependencies=TRUE)
install.packages("gridExtra", dependencies=TRUE)

# load packages
library(RedditExtractoR)
library(stringr)
library(dplyr)
library(ggplot2)
library(tidytext)
library(textdata)
library(gridExtra)

# fix text data
textdata::lexicon_nrc(delete=TRUE)
textdata::lexicon_nrc()

# create a dataset
reddit_data <- find_thread_urls(subreddit="schizophrenia", 
                         sort_by="top", 
                         period="all")

# access titles
reddit_data$title

# sentiment lexicon
get_sentiments(lexicon=c("nrc"))

# create tibbles
text_df <- tibble(line=1:1001, text=reddit_data$title)
head(text_df)

# text processing
text_tidy <- text_df %>%
  unnest_tokens(word, text)

data(stop_words)
text_tidy <- text_tidy %>%
  anti_join(stop_words)

# sentiment analysis
afinn_word_counts <- text_tidy %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

# plotting graph
afinn_word_counts %>%
  filter(n > 5) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col() +
  coord_flip() +
  labs(y = "sentiment (n)") +
  ggtitle("Sentiment from SCZ Subreddit")

# distribution of sentiments
afinn_word_counts %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment) %>%
  ggplot(aes(sentiment, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  ggtitle("Sentiments (Distribution)") +
  coord_flip()

# modelling sentiments
nrc_word_counts <- text_tidy %>%
  inner_join(get_sentiments("nrc")) %>%
  count(line, sentiment, sort = TRUE) %>%
  ungroup()

ggplot(data = nrc_word_counts, mapping = aes(x = line, y = n)) +
  geom_smooth(method="loess", formula="y~x", span=0.2) +
  xlab("document (line)") +
  ylab("sentiment (conditional mean)") +
  ggtitle("Intertemporal Use of Sentiments (Conditional Mean)")

# Intertemporal Use of Sentiments (Score)

# load bing lexicon
bing_word_counts <- bind_rows(
  text_tidy %>%
    inner_join(get_sentiments("bing")) %>%
    mutate(method = "Bing et al."))


sentiment_sum <- ifelse(bing_word_counts$sentiment == "positive", 1, -1)
sentiment_sum_df <- cbind(bing_word_counts$line, sentiment_sum)
colnames(sentiment_sum_df) <- c('var1', 'var2')
sentiment_sum_df <- as.data.frame(sentiment_sum_df)
sentiment_sum_df <- aggregate(sentiment_sum_df$var2,
                              by=list(line=sentiment_sum_df$var1), FUN=sum)

# plot graph
ggplot(data = sentiment_sum_df, mapping = aes(x = line, y = x)) +
  geom_smooth(method="loess", formula="y~x", span=0.2) +
  xlab("document (line)") +
  ylab("sentiment (score)") +
  ggtitle("Intertemporal Use of Sentiments (Score)")