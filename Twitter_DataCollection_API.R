# ================================
# PAGE 1: Twitter API Data Collection
# Collect Tweets from Official Turkish Accounts
# ================================

# Load required libraries
library(rtweet)
library(dplyr)

# ----------------
# 1. Twitter API Authentication
# ----------------
# Set up your Twitter Developer credentials (replace with your actual keys)
twitter_token <- rtweet::create_token(
  app = "YourAppName",                # Replace with your app name
  consumer_key = "your_consumer_key", # Replace with your key
  consumer_secret = "your_consumer_secret", # Replace with your secret
  access_token = "your_access_token",       # Replace with token
  access_secret = "your_access_secret"      # Replace with secret
)

# ----------------
# 2. Define Target Accounts
# ----------------
accounts <- c(
  "TC_Basbakan",
  "TIKA",
  "RT_Erdogan",
  "ByegmENG",
  "trpresidency",
  "MFATurkey",
  "MevlutCavusoglu"
)

# ----------------
# 3. Collect Tweets for Each Account (up to 3200 per account)
# ----------------
# Create an empty list to store data
all_tweets <- list()

for (account in accounts) {
  cat("Fetching tweets for:", account, "\n")
  
  tweets <- rtweet::get_timeline(
    user = account,
    n = 3200,
    token = twitter_token,
    include_rts = TRUE
  )
  
  all_tweets[[account]] <- tweets
  Sys.sleep(3)  # Avoid hitting rate limits
}

# ----------------
# 4. Combine All Tweets into One Data Frame
# ----------------
combined_tweets <- bind_rows(all_tweets, .id = "account_name")

# Optional: Filter by date range (Sept 1, 2017 – Jan 1, 2018)
combined_tweets_filtered <- combined_tweets %>%
  filter(created_at >= as.POSIXct("2017-09-01") & created_at < as.POSIXct("2018-01-01"))

# ----------------
# 5. Save Raw Data to CSV
# ----------------
write.csv(combined_tweets_filtered, "turkey_public_diplomacy_tweets.csv", row.names = FALSE)

# Done! 