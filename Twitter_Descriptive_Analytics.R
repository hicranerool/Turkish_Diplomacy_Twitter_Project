# ================================
# PAGE 2: Descriptive Analytics
# Summary Statistics and Visualizations
# ================================

# Load required libraries
library(dplyr)
library(ggplot2)
library(lubridate)
library(readr)

# ----------------
# 1. Load the Cleaned Tweet Dataset
# ----------------
tweets <- read_csv("turkey_public_diplomacy_tweets.csv")

# ----------------
# 2. Basic Tweet Counts per Account
# ----------------
tweet_counts <- tweets %>%
  group_by(account_name) %>%
  summarise(total_tweets = n())

print(tweet_counts)

# ----------------
# 3. Engagement Statistics per Account
# ----------------
engagement_stats <- tweets %>%
  group_by(account_name) %>%
  summarise(
    avg_likes = mean(favorite_count, na.rm = TRUE),
    avg_retweets = mean(retweet_count, na.rm = TRUE),
    median_likes = median(favorite_count, na.rm = TRUE),
    median_retweets = median(retweet_count, na.rm = TRUE)
  )

print(engagement_stats)

# ----------------
# 4. Most Liked and Most Retweeted Tweets (Top 5)
# ----------------
top_liked <- tweets %>%
  arrange(desc(favorite_count)) %>%
  select(account_name, screen_name, created_at, text, favorite_count) %>%
  head(5)

top_retweeted <- tweets %>%
  arrange(desc(retweet_count)) %>%
  select(account_name, screen_name, created_at, text, retweet_count) %>%
  head(5)

print(top_liked)
print(top_retweeted)

# ----------------
# 5. Daily Tweet Activity by Account
# ----------------
tweets_by_day <- tweets %>%
  mutate(date = as.Date(created_at)) %>%
  group_by(account_name, date) %>%
  summarise(daily_tweets = n()) %>%
  ungroup()

# ----------------
# 6. Plot: Daily Tweet Volume Over Time
# ----------------
ggplot(tweets_by_day, aes(x = date, y = daily_tweets, color = account_name)) +
  geom_line(size = 1) +
  labs(
    title = "Daily Tweet Volume by Official Accounts",
    x = "Date",
    y = "Number of Tweets",
    color = "Account"
  ) +
  theme_minimal()

# ----------------
# 7. Plot: Total Tweets per Account (Bar Chart)
# ----------------
ggplot(tweet_counts, aes(x = reorder(account_name, -total_tweets), y = total_tweets)) +
  geom_bar(stat = "identity", fill = "#2c3e50") +
  labs(
    title = "Total Tweets per Account (Sept 2017 – Jan 2018)",
    x = "Account",
    y = "Tweet Count"
  ) +
  theme_minimal() +
  coord_flip()

