# Explore correlations between topic distributions and properties
# of the user's social graph.

# 0: libraries ------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(glue)

# 1: external function definitions ----------------------------------------

source("R/network_measures/katz_powell.R")
source("R/network_measures/weighted_mutuality.R")
source("R/network_construction/filter_graph.R")
source("R/reading_data/read_snapshots.R")

# 2: helper functions -----------------------------------------------------

# This wrapper accepts a dataframe of ego-topics and pulls out the topics.
filter_graph_wrapper <- function(graph, ego) {
  filter_graph(graph, ego$topic)
}

# 3: module constants -----------------------------------------------------

source("R/constants.R")
MIN_OBS <- 5

# 4: load external objects ------------------------------------------------

load(glue("output/{USERNAME}-graph_df.rda"))
load(glue("output/{USERNAME}_corr-df.rda"))
load(glue("output/{USERNAME}-graph_analysis.rda"))

# 5: compute measures on the graph data -----------------------------------

# all snapshots for one ego
ego_topics <- read_ego_topics(USERNAME)

# construct subgraph and statistics
graphs_and_topics <-
  graph_data %>% 
  left_join(ego_topics, by = c("year", "month", "author")) %>%
  rename(ego_topics = data) %>% 
  drop_na() %>% 
  mutate(
    date = make_date(year, month),
    topic_subgraph = map2(graph, ego_topics, filter_graph_wrapper),
    topical_overlap = map2_dbl(topical_subgraph, graph, function(a, b) {
      # compute ratio of edges of two graphs
      
      if (ecount(a) == 0 | ecount(b) == 0) {
        return(NA)
      }
      
      ecount(a) / ecount(b)
    })
  ) %>% 
  drop_na()

# 6: plot data ------------------------------------------------------------

# graph statistics and topic distributions
graphs_and_proportions <- corr_df %>%
  left_join(graphs_and_topics, by = c("year", "month")) %>%
  select(date,
         topic,
         prop_ego,
         prop_alters,
         topical_overlap,
         relative_topic_reciprocity)

graphs_and_proportions %>% 
  group_by(topic) %>% 
  filter(n() > MIN_OBS) %>% 
  ggplot(mapping = aes(prop_ego, topical_overlap, colour = topic)) +
  geom_point(alpha = P_ALPHA) +
  geom_smooth(method = "lm") +
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    x = "prop. of Ego's posts",
    y = "topical edges in complete graph",
    title = "Proportion of topical edges in graph vs prop. of posts to a topic",
    caption = glue("Minimum number of observations: {MIN_OBS}")
  ) +
  facet_wrap(~topic)

ggsave(filename = glue("figs/{USERNAME}-WEIGHTS-prop-vs-overlap.png"))

graphs_and_proportions %>%
  group_by(topic) %>% 
  filter(n() > MIN_OBS) %>% 
  ggplot(mapping = aes(prop_ego, relative_topic_reciprocity, colour = topic)) +
  geom_point(alpha = P_ALPHA) +
  geom_smooth() +
  #scale_y_continuous(limits = c(-1, 1)) +
  labs(
    x = "prop. of Ego's posts",
    y = "relative reciprocity of subgraph",
    title = "Difference of in/out edges relative to all edges, vs. prop of posts",
    caption = glue("Minimum number of observations: {MIN_OBS}")
  ) +
  facet_wrap(~topic)

ggsave(filename = glue("figs/{USERNAME}-WEIGHTS-prop-vs-relative-reciprocity.png"))

weight_mutual_edges <-
  graphs_and_topics %>%
  left_join(corr_df, by = c("year", "month")) %>% 
  group_by(topic) %>%
  mutate(
    t = prop_ego * relative_topic_reciprocity
  ) %>% 
  ungroup()

weight_mutual_edges %>%
  group_by(topic) %>% 
  filter(n() > MIN_OBS) %>% 
  ggplot(mapping = aes(make_date(year, month), t, colour = topic)) +
  geom_point(alpha = P_ALPHA) +
  geom_smooth(method = "lm") +
  facet_wrap( ~ topic) +
  labs(
    x = "",
    y = "weighted reciprocity",
    title = "Weight edge difference by proportion of posts",
    caption = glue("Minimum number of observations: {MIN_OBS}")
  )
# new
weight_mutual_edges %>%
  group_by(topic) %>% 
  filter(n() > MIN_OBS) %>% 
  ggplot(mapping = aes(prop_ego, t, colour = topic)) +
  geom_point(alpha = P_ALPHA) +
  geom_smooth(method = "lm") +
  geom_abline() +
  facet_wrap( ~ topic) +
  labs(
    x = "prop ego",
    y = "weighted reciprocity",
    title = "Weight edge difference by proportion of posts",
    caption = glue("Minimum number of observations: {MIN_OBS}")
  )

ggsave(filename = glue("figs/{USERNAME}-WEIGHTS-weighted-relative-reciprocity.png"))

weight_mutual_edges %>% 
  group_by(topic) %>% 
  filter(n() > MIN_OBS) %>% 
  ggplot(mapping = aes(t)) +
  geom_point(mapping = aes(y = prop_ego, colour = "Ego"), alpha = P_ALPHA) +
  #geom_smooth(mapping = aes(y = prop_ego, colour = "Ego")) +
  geom_point(mapping = aes(y = prop_alters, colour = "Alters"), alpha = P_ALPHA) +
  #geom_smooth(mapping = aes(y = prop_ego, colour = "Alters")) +
  facet_wrap(~topic) +
  labs(
    x = "probability-weighted reciprocity",
    y = "proportion of posts by ego"
  )

ggsave(filename = glue("figs/{USERNAME}-WEIGHTS-prop-vs-weighted-reciprocity.png"))

graphs_and_topics %>%
  mutate(cs = cumsum(topical_overlap)) %>%
  ggplot(mapping = aes(date, cs)) +
  geom_line() +
  labs(x = "",
       y = "cumsum",
       title = "Cumsum of ratio of topical edges")

ggsave(filename = glue("figs/{USERNAME}-WEIGHTS-cumsum-topical-overlap.png"))

