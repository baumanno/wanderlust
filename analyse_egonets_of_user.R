
# 0: Libraries ------------------------------------------------------------

library(tidyverse)
library(glue)
library(gridExtra)
library(igraph)
library(lubridate)

# 1: External functions ---------------------------------------------------

source("R/reading_data/read_snapshots.R")
source("R/network_construction/build_from_edgelist.R")
source("R/network_measures/katz_powell.R")
source("R/network_measures/weighted_mutuality.R")
source("R/network_measures/krackhardt.R")

# 2: global constants -----------------------------------------------------

# monocasa formido cavedave IronWolve
user <- "IronWolve"

# 3: Read data from snapshots ---------------------------------------------

edgelists <- read_edgelist(user)
topics_of_alters <- read_alters_topics(user)

# the ego is missing from the alters snapshots, so we add it manually with a
# special topic of -1

fix_missing_ego <- function(df) {
  if (nrow(df) == 0) {
    return(df)
  }
  add_row(df, author = user, topic = -1)
}

topics_of_alters <-
  topics_of_alters %>%
  mutate(data = map(data, fix_missing_ego))

# 4: aggregate data into single dataframe ---------------------------------

# store edgelists and topic metadata in a frame
graph_data <-
  left_join(edgelists, topics_of_alters, by = c("month", "year", "author")) %>%
  select(year,
         month,
         author,
         edgelist = data.x,
         alters_topics = data.y)

# remove the now unused objects
rm(edgelists, topics_of_alters)

# 5: analysis of egonetwork -----------------------------------------------

graph_data <- graph_data %>%
  mutate(graph = map2(edgelist, alters_topics, egonet_from_edgelist))

graph_analysis <- graph_data %>%
  mutate(
    vertices = map_int(graph, vcount),
    katz_powell = map_dbl(graph, katz_powell_mutuality),
    weighted = map_dbl(graph, weighted_mutuality, node = author)
  )

# 6: store result objects -------------------------------------------------

save(graph_data, file = glue("output/{user}-graph_df.rda"))
save(graph_analysis, file = glue("output/{user}-graph_analysis.rda"))

# 7: plot data ------------------------------------------------------------

(
  a <- graph_analysis %>%
    filter(vertices > 0) %>%
    ggplot(mapping = aes(make_date(year, month), vertices)) +
    geom_point(alpha = 1 / 10) +
    geom_smooth() +
    labs(x = "", y = "#vertices", title = user)
)

(
  b <- graph_analysis %>%
    ggplot(mapping = aes(make_date(year, month), katz_powell)) +
    geom_point(aes(colour = katz_powell), alpha = 1 / 10) +
    geom_smooth() +
    labs(x = "", y = "Katz-Powell-index")
)

(
  c <- graph_analysis %>%
    ggplot(mapping = aes(make_date(year, month), weighted)) +
    geom_point(aes(colour = weighted), alpha = 1 / 10) +
    geom_smooth() +
    scale_y_continuous(limits = c(-1, 1)) +
    labs(x = "", y = "weighted mututality", colour = "")
)

g_a <- grid.arrange(a, b, c, nrow = 3)

(
  d <- graph_analysis %>%
    ggplot(mapping = aes(vertices, katz_powell)) +
    geom_point(colour = "blue", alpha = 1 / 10) +
    geom_smooth() +
    labs(x = "#vertices", y = "Katz-Powell-index")
)

(
  e <- graph_analysis %>%
    ggplot(mapping = aes(vertices, weighted)) +
    geom_point(colour = "blue", alpha = 1 / 10) +
    geom_smooth() +
    scale_y_continuous(limits = c(-1, 1)) +
    labs(x = "#vertices", y = "weighted mutuality")
)

g_b <- grid.arrange(d, e, nrow = 2)

p <- grid.arrange(g_a, g_b, ncol = 2)

ggsave(glue("figs/{user}-graph-analysis.png"), p)
