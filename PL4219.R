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
