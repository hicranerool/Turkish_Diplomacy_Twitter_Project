# ================================
# PAGE 5: Sentiment Analysis
# Polarity & Emotional Tone Classification
# ================================

# Load required libraries
library(dplyr)
library(tidytext)
library(readr)
library(ggplot2)
library(stringr)
library(tidyr)
library(textdata)  # for lexicons
library(syuzhet)   # alternative sentiment scoring

# ----------------
# 1. Load Dataset
# ----------------
tweets <- read_csv("turkey_public_diplomacy_tweets.csv")

# ----------------
# 2. Clean and Tokenize
# ----------------
clean_tweets <- tweets %>%
  select(status_id, account_name, text) %>%
  mutate(text = str_remove_all(text, "http\\S+|https\\S+|@\\S+|#\\S+")) %>%
  unnest_tokens(word, text)

# Remove stopwords
clean_words <- clean_tweets %>%
  anti_join(stop_words, by = "word")

# ----------------
# 3. Load Sentiment Lexicon (AFINN or Bing)
# ----------------
# Option 1: Bing lexicon (positive/negative)
bing_sentiments <- get_sentiments("bing")

sentiment_scores <- clean_words %>%
  inner_join(bing_sentiments, by = "word") %>%
  count(account_name, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>%
  mutate(net_sentiment = positive - negative)

# View sentiment by account
print(sentiment_scores)

# ----------------
# 4. Plot Net Sentiment by Account
# ----------------
ggplot(sentiment_scores, aes(x = reorder(account_name, net_sentiment), y = net_sentiment, fill = net_sentiment > 0)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(
    title = "Net Sentiment Score by Account",
    x = "Account",
    y = "Net Sentiment (Positive - Negative)"
  ) +
  scale_fill_manual(values = c("TRUE" = "#27ae60", "FALSE" = "#c0392b")) +
  theme_minimal()

# ----------------
# 5. Optional: Emotion Categories with NRC
# ----------------
nrc_sentiments <- get_sentiments("nrc") %>%
  filter(sentiment %in% c("anger", "joy", "sadness", "fear", "trust", "disgust"))

emotion_scores <- clean_words %>%
  inner_join(nrc_sentiments, by = "word") %>%
  count(account_name, sentiment) %>%
  arrange(desc(n))

# Plot Emotion Categories
ggplot(emotion_scores, aes(x = sentiment, y = n, fill = account_name)) +
  geom_col(position = "dodge") +
  labs(
    title = "Emotion Categories Across Accounts",
    x = "Emotion",
    y = "Frequency",
    fill = "Account"
  ) +
  theme_minimal()

# ----------------
# 6. Optional: Sentence-Level Polarity with syuzhet
# ----------------
tweets$syuzhet_score <- get_sentiment(tweets$text, method = "syuzhet")

# Plot: Distribution of Sentiment Scores
ggplot(tweets, aes(x = syuzhet_score)) +
  geom_histogram(fill = "#2980b9", bins = 30) +
  labs(
    title = "Distribution of Sentiment Scores (Syuzhet)",
    x = "Sentiment Score",
    y = "Tweet Count"
  ) +
  theme_minimal()
