# ================================
# PAGE 3: Temporal Patterns
# Analyzing Temporal Trends in Tweet Activity
# ================================

# Load required libraries
library(dplyr)
library(ggplot2)
library(lubridate)
library(readr)

# ----------------
# 1. Load the Tweet Dataset
# ----------------
tweets <- read_csv("turkey_public_diplomacy_tweets.csv")

# ----------------
# 2. Add Time Variables
# ----------------
tweets <- tweets %>%
  mutate(
    date = as.Date(created_at),
    weekday = wday(created_at, label = TRUE),
    hour = hour(created_at),
    week = floor_date(created_at, "week")
  )

# ----------------
# 3. Weekly Tweet Volume
# ----------------
weekly_activity <- tweets %>%
  group_by(account_name, week) %>%
  summarise(weekly_tweets = n()) %>%
  ungroup()

# Plot: Weekly Tweet Volume
ggplot(weekly_activity, aes(x = week, y = weekly_tweets, color = account_name)) +
  geom_line(size = 1) +
  labs(
    title = "Weekly Tweet Activity by Account",
    x = "Week",
    y = "Tweet Count",
    color = "Account"
  ) +
  theme_minimal()

# ----------------
# 4. Hourly Tweet Distribution
# ----------------
hourly_dist <- tweets %>%
  group_by(account_name, hour) %>%
  summarise(hourly_tweets = n()) %>%
  ungroup()

# Plot: Hourly Tweet Activity (Across All Accounts)
ggplot(hourly_dist, aes(x = hour, y = hourly_tweets, fill = account_name)) +
  geom_col(position = "dodge") +
  labs(
    title = "Hourly Distribution of Tweets",
    x = "Hour of Day (0–23)",
    y = "Number of Tweets",
    fill = "Account"
  ) +
  theme_minimal()

# ----------------
# 5. Weekday Posting Patterns
# ----------------
weekday_dist <- tweets %>%
  group_by(account_name, weekday) %>%
  summarise(weekday_tweets = n()) %>%
  ungroup()

# Plot: Tweets by Day of Week
ggplot(weekday_dist, aes(x = weekday, y = weekday_tweets, fill = account_name)) +
  geom_col(position = "dodge") +
  labs(
    title = "Tweet Frequency by Day of the Week",
    x = "Day of Week",
    y = "Number of Tweets",
    fill = "Account"
  ) +
  theme_minimal()

