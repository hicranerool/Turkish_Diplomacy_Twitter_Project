# ================================
# PAGE 6: Network Analysis
# Mention and Retweet Networks
# ================================

# Load required libraries
library(dplyr)
library(readr)
library(stringr)
library(tidyr)
library(igraph)
library(ggraph)
library(tidygraph)

# ----------------
# 1. Load Dataset
# ----------------
tweets <- read_csv("turkey_public_diplomacy_tweets.csv")

# ----------------
# 2. Mention Network
# ----------------
# Extract mentions
mentions_df <- tweets %>%
  select(screen_name, text) %>%
  mutate(mentions = str_extract_all(text, "@\\w+")) %>%
  unnest(mentions) %>%
  filter(!is.na(mentions)) %>%
  mutate(mentions = str_remove_all(mentions, "[^@\\w]")) %>%
  group_by(screen_name, mentions) %>%
  summarise(weight = n(), .groups = "drop")

# Create igraph object
mention_graph <- graph_from_data_frame(mentions_df, directed = TRUE)

# Convert to tidygraph for visualization
mention_tidy <- as_tbl_graph(mention_graph)

# Plot Mention Network
ggraph(mention_tidy, layout = "fr") +
  geom_edge_link(aes(width = weight), alpha = 0.4, color = "#3498db") +
  geom_node_point(size = 5, color = "#2c3e50") +
  geom_node_text(aes(label = name), repel = TRUE, size = 3) +
  theme_void() +
  labs(title = "Mention Network of Official Turkish Accounts")

# ----------------
# 3. Retweet Network
# ----------------
# Extract retweet structure
retweet_df <- tweets %>%
  filter(str_detect(text, "^RT @")) %>%
  mutate(
    retweeter = screen_name,
    original = str_extract(text, "(?<=RT @)\\w+")
  ) %>%
  filter(!is.na(original)) %>%
  group_by(retweeter, original) %>%
  summarise(weight = n(), .groups = "drop")

# Create retweet graph
retweet_graph <- graph_from_data_frame(retweet_df, directed = TRUE)
retweet_tidy <- as_tbl_graph(retweet_graph)

# Plot Retweet Network
ggraph(retweet_tidy, layout = "fr") +
  geom_edge_link(aes(width = weight), alpha = 0.4, color = "#e67e22") +
  geom_node_point(size = 5, color = "#34495e") +
  geom_node_text(aes(label = name), repel = TRUE, size = 3) +
  theme_void() +
  labs(title = "Retweet Network of Turkish Public Diplomacy Accounts")

# ----------------
# 4. Identify Central Nodes
# ----------------
mention_centrality <- data.frame(
  node = V(mention_graph)$name,
  degree = degree(mention_graph, mode = "in")
) %>%
  arrange(desc(degree)) %>%
  head(10)

retweet_centrality <- data.frame(
  node = V(retweet_graph)$name,
  degree = degree(retweet_graph, mode = "in")
) %>%
  arrange(desc(degree)) %>%
  head(10)

# View central actors
print("Top Mentioned Accounts:")
print(mention_centrality)

print("Top Retweeted Accounts:")
print(retweet_centrality)
