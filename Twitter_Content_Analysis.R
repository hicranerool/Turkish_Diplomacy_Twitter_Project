# ================================
# PAGE 4: Content Analysis
# Extracting Frequent Terms, Hashtags, Mentions
# ================================

# Load required libraries
library(dplyr)
library(tidytext)
library(ggplot2)
library(stringr)
library(readr)
library(tidyr)
library(wordcloud)

# ----------------
# 1. Load Tweet Dataset
# ----------------
tweets <- read_csv("turkey_public_diplomacy_tweets.csv")

# ----------------
# 2. Tokenize Tweets into Words
# ----------------
# Remove URLs, mentions, hashtags for cleaner tokenization
clean_tweets <- tweets %>%
  mutate(text = str_remove_all(text, "http\\S+|https\\S+|@\\S+|#\\S+"))

# Tokenization
words_df <- clean_tweets %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words, by = "word")  # remove common English stopwords

# ----------------
# 3. Most Frequent Words
# ----------------
top_words <- words_df %>%
  count(word, sort = TRUE) %>%
  filter(n > 30)

# Plot: Top Words
ggplot(top_words, aes(x = reorder(word, n), y = n)) +
  geom_col(fill = "#2c3e50") +
  coord_flip() +
  labs(
    title = "Most Frequent Words in Tweets",
    x = "Word",
    y = "Frequency"
  ) +
  theme_minimal()

# ----------------
# 4. Word Cloud
# ----------------
wordcloud(
  words = top_words$word,
  freq = top_words$n,
  max.words = 100,
  colors = brewer.pal(8, "Dark2"),
  random.order = FALSE
)

# ----------------
# 5. Hashtag Extraction and Frequency
# ----------------
hashtags_df <- tweets %>%
  mutate(hashtags = str_extract_all(text, "#\\S+")) %>%
  unnest(hashtags) %>%
  group_by(hashtags) %>%
  summarise(freq = n()) %>%
  arrange(desc(freq)) %>%
  filter(freq > 10)

# Plot: Top Hashtags
ggplot(hashtags_df, aes(x = reorder(hashtags, freq), y = freq)) +
  geom_col(fill = "#1da1f2") +
  coord_flip() +
  labs(
    title = "Most Common Hashtags",
    x = "Hashtag",
    y = "Frequency"
  ) +
  theme_minimal()

# ----------------
# 6. Mention Extraction and Frequency
# ----------------
mentions_df <- tweets %>%
  mutate(mentions = str_extract_all(text, "@\\S+")) %>%
  unnest(mentions) %>%
  group_by(mentions) %>%
  summarise(freq = n()) %>%
  arrange(desc(freq)) %>%
  filter(freq > 10)

# Plot: Most Mentioned Accounts
ggplot(mentions_df, aes(x = reorder(mentions, freq), y = freq)) +
  geom_col(fill = "#e74c3c") +
  coord_flip() +
  labs(
    title = "Most Mentioned Twitter Accounts",
    x = "Mention",
    y = "Frequency"
  ) +
  theme_minimal()
